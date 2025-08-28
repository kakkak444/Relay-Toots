{-  Copyright © 2025 kakkak444.

    This Source Code Form is subject to the terms of the Mozilla Public
    License, v. 2.0. If a copy of the MPL was not distributed with this
    file, You can obtain one at https://mozilla.org/MPL/2.0/.
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import App                                        hiding (LogLevel(..))
import App                        qualified as AP
import Control.Monad
import Control.Monad.Reader
import Data.Bifunctor                                    (first)
import Data.Event
import Data.Text                  qualified as T
import Data.Text.IO               qualified as T
import Data.Time.Clock
import Data.Twitter                         as TW
import Data.Vector                qualified as V
import Network.HTTP.Types.Status
import Network.Wai
import Network.Wai.Handler.Warp
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


runWithEnv :: (MonadIO m) => ReaderT Env m a -> m a
runWithEnv app = loadEnv >>= runReaderT app

type HealthCheckAPI  = Get '[PlainText] T.Text
type ServerAPI = HealthCheckAPI :<|> TootReceiverAPI

serverApi :: Proxy ServerAPI
serverApi = Proxy

healthCheck :: AppT Handler T.Text
healthCheck = do
    liftIO $ putStrLn "health checked !"
    return $ T.pack "healthy"


readToken :: (MonadIO m, MonadFail m, HasTokenFilePath m, HasLogger m) => m Token
readToken = do
    logDebug "reading token from file..."

    file <- askTokenFilePath
    accessToken : refreshToken : _ <- T.lines <$> liftIO (T.readFile file)
    return $ Token { accessToken, refreshToken, expiresIn = Nothing }

writeToken :: (MonadIO m, HasTokenFilePath m, HasLogger m) => Token -> m ()
writeToken token = do
    logDebug "writing token to file..."

    file <- askTokenFilePath
    liftIO $ T.writeFile file $ T.unlines [accessToken token, refreshToken token]

server :: (Event -> AppT IO ()) -> ServerT ServerAPI (AppT Handler)
server cont = healthCheck :<|> tootReceiver cont

canReadWrite :: (MonadIO m) => FilePath -> m Bool
canReadWrite file = liftIO $ fileAccess file True True False

checkTokenFile :: (MonadIO m, HasTokenFilePath m, HasLogger m) => m ()
checkTokenFile = do
    logDebug "token file checking..."

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

warpLogger :: LogChan -> Request -> Status -> Maybe Integer -> IO ()
warpLogger logChan req status _ = runLoggingT logChan $ logInfo $ T.intercalate " " [clientIP, method, path, ver, sc]
  where
    method   = T.show $ requestMethod req
    ver      = T.show $ httpVersion   req
    clientIP = T.show $ remoteHost    req
    sc       = T.show $ statusCode status
    path     = "/" <> T.intercalate "/" (pathInfo req)

main :: IO ()
main = do
    runStdoutLoggerT AP.TRACE $ do
        runWithEnv $ do
            checkTokenFile

            initToken <- readToken
            tokenRef <- tokenRefresher 5 (\token -> logInfo "token refreshed" >> writeToken token) initToken

            logInfo "server started"

            port <- askPort
            env  <- ask
            logChan <- askLogChan
            let settings = setPort port . setLogger (warpLogger logChan) $ defaultSettings
            liftIO $ runSettings settings $ serve serverApi $ hoistServer serverApi (runLoggingT logChan . runApp env tokenRef) (server relay)

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
        tag <- T.toLower <$> askCatchingTag
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

htmlToPlain :: T.Text -> Either T.Text T.Text
htmlToPlain html = first T.show $ runPure $ readHtml def html >>= writePlain def
