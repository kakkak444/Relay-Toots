{-  Copyright © 2025 kakkak444.

    This Source Code Form is subject to the terms of the Mozilla Public
    License, v. 2.0. If a copy of the MPL was not distributed with this
    file, You can obtain one at https://mozilla.org/MPL/2.0/.
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude                                    hiding (truncate)
import Prelude                    qualified as P

import App
import Control.Monad
import Control.Monad.Reader
import Data.Bifunctor                                    (first)
import Data.Event
import Data.Text                  qualified as T
import Data.Text.IO               qualified as T
import Data.Time.Clock
import Data.Twitter                         as TW
import Data.Vector                qualified as V
import Network.Wai.Handler.Warp
import Network.Wai.Logger                                (withStdoutLogger)
import PostSender                           as PS
import Servant
import System.Exit
import System.Posix.Files                                (fileAccess, fileExist)
import Text.Pandoc.Class                                 (runPure)
import Text.Pandoc.Options
import Text.Pandoc.Readers.HTML
import Text.Pandoc.Writers
import TokenRefresher
import TootReceiver
import UnliftIO                                   hiding (Handler)
import UnliftIO.Concurrent


runWithEnv :: (MonadIO m) => ReaderT Env m a -> m a
runWithEnv app = loadEnv >>= runReaderT app


type HealthCheckAPI  = Get '[PlainText] T.Text
type ServerAPI = HealthCheckAPI :<|> TootReceiverAPI

healthCheckApi :: Proxy HealthCheckAPI
healthCheckApi = Proxy

serverApi :: Proxy ServerAPI
serverApi = Proxy

healthCheck :: AppT Handler T.Text
healthCheck = do
    liftIO $ putStrLn "health checked !"
    return $ T.pack "healthy"


readToken :: (MonadIO m, MonadFail m, HasTokenFilePath m) => m Token
readToken = do
    file <- askTokenFilePath
    accessToken : refreshToken : _ <- T.lines <$> liftIO (T.readFile file)
    return $ Token { accessToken, refreshToken, expiresIn = Nothing }

writeToken :: (MonadIO m, HasTokenFilePath m) => Token -> m ()
writeToken token = do
    file <- askTokenFilePath
    liftIO $ T.writeFile file $ T.unlines [accessToken token, refreshToken token]

server :: (Event -> AppT IO ()) -> ServerT ServerAPI (AppT Handler)
server cont = healthCheck :<|> tootReceiver cont


logInfo :: (MonadIO m) => T.Text -> m ()
logInfo text = liftIO $ T.putStrLn $ "[INFO] " <> text

logError :: (MonadIO m) => T.Text -> m ()
logError text = liftIO $ T.putStrLn $ "[ERROR] " <> text

canReadWrite :: (MonadIO m) => FilePath -> m Bool
canReadWrite file = liftIO $ fileAccess file True True False

checkTokenFile :: (MonadIO m, HasTokenFilePath m) => m ()
checkTokenFile = do
    tokenFile <- askTokenFilePath
    exists <- liftIO $ fileExist tokenFile
    unless exists $ do
        logError "Token file does not exist."
        liftIO exitFailure

    has_proper_permission <- canReadWrite tokenFile
    unless has_proper_permission $ do
        logError "Token file does not have proper permission (the file must have read and write permission)."
        liftIO exitFailure

relay :: Event -> AppT IO ()
relay event = do
    logInfo "event received"
    locked <- tweetIsLocked
    if not locked then do
        res <- sendToot $ eventObject event
        case res of
            Right () -> return ()
            Left (TooManyRequests Nothing) -> do
                logInfo $ "reach rate limit. block until" <> T.show (15 * 60 :: Int)
                lockTweet $ 15 * 60
            Left (TooManyRequests (Just reset)) -> do
                logInfo $ "reach rate limit. block until" <> T.show (show reset)
                currTime <- liftIO $ getCurrentTime
                lockTweet $ diffUTCTime reset currTime
            Left PS.Unauthorized -> logInfo "token may be expired"
            Left (Other msg) -> logInfo msg
    else
        logInfo "now blocking"

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    runWithEnv $ do
        checkTokenFile

        initToken <- readToken
        tokenRef <- tokenRefresher 5 (\token -> logInfo "token refreshed" >> writeToken token) initToken

        logInfo "server started"

        port <- askPort
        env  <- ask
        liftIO $ withStdoutLogger $ \logger -> do
            let settings = setPort port . setLogger logger $ defaultSettings
            runSettings settings $ serve serverApi $ hoistServer serverApi (runApp env tokenRef) (server relay)

threadDelay' :: NominalDiffTime -> IO ()
threadDelay' diff = threadDelay $ P.truncate $ diff * 10 ^ (6 :: Int)

sendToot :: (MonadIO m) => Toot -> AppT m (Either SendingPostError ())
sendToot toot = do
    contain <- containTargetTag
    isFrom  <- isFromTargetUser

    targetTag <- askCatchingTag
    case tootToTweet targetTag toot of
        Just tweet'
            | contain && isFrom -> tweet tweet'
        _ -> return $ Right ()
  where
    containTargetTag = do
        tag <- askCatchingTag
        return $ V.elem tag . V.map T.toLower . V.map tagName $ tootTags toot

    isFromTargetUser = do
        userName <- askTargetUser
        return $ userName == acctUsername (tootAccount toot)


-- sendToot' :: (MonadIO m) => T.Text -> T.Text -> Token -> Toot -> m (Either SendingPostError ())
-- sendToot' targetUser catchingTag toot
--     | V.elem catchingTag . V.map T.toLower . V.map tagName $ tootTags toot
--     , targetUser == acctUsername (tootAccount toot)
--     , Just tweet' <- tootToTweet catchingTag toot
--         = tweet tweet'
--     | otherwise = return $ Right ()

tootToTweet :: T.Text -> Toot -> Maybe TW.Post
tootToTweet catchingTag (Toot { tootContent = content, tootUrl = _url }) =
    let content' = either (const Nothing) (Just . removeHashTag catchingTag) $ htmlToPlain $ content
    in
        mkPost <$> content'

removeHashTag :: T.Text -> T.Text -> T.Text
removeHashTag tagName content = T.unlines . filter ((/= ("#" <> T.toLower tagName)) . T.toLower) . T.lines $ content

addUrl :: URI -> T.Text -> T.Text
addUrl url text = text <> "\n\n" <> "[ｆｒｏｍ]:" <> T.show url

htmlToPlain :: T.Text -> Either T.Text T.Text
htmlToPlain html = first T.show $ runPure $ readHtml def html >>= writePlain def

truncate :: Int -> T.Text -> T.Text
truncate len text =
    if T.length text <= len then
        text
    else
        text'
  where
    text' = T.take (len - 3) text <> "..."
