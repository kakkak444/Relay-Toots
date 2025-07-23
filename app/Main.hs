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

import Control.Concurrent
import Control.Exception                                 (bracket_)
import Control.Monad
import Control.Monad.IO.Class                            (liftIO)
import Data.Bifunctor                                    (first)
import Data.ByteString            qualified as BS
import Data.ByteString.Char8      qualified as BS8
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
import System.Environment                                (getEnv)
import System.Environment.Blank                          (getEnvDefault)
import System.Exit
import System.IO
import System.Posix.Files                                (fileAccess, fileExist)
import Text.Pandoc.Class                                 (runPure)
import Text.Pandoc.Options
import Text.Pandoc.Readers.HTML
import Text.Pandoc.Writers
import TokenRefresher
import TootReceiver


data Env = Env
    { secretKey     :: BS.ByteString
    , credential    :: Credential
    , targetUser    :: T.Text
    , catchingTag   :: T.Text
    , tokenFilePath :: FilePath
    , port          :: Int
    }

loadEnv :: IO Env
loadEnv = do
    secretKey    <- BS8.pack <$> getEnv "WEBHOOK_SECRET_KEY"

    clientId     <- T.pack   <$> getEnv "X_API_CLIENT_ID"
    clientSecret <- T.pack   <$> getEnv "X_API_CLIENT_SECRET"

    catchingTag   <- T.toLower . T.pack <$> getEnv "RELAY_TAG"
    targetUser    <- T.pack <$> getEnv "TARGET"
    tokenFilePath <- getEnv "TOKEN_PATH"

    port <- readIO =<< getEnvDefault "PORT" "3000"

    return $ Env { secretKey
                 , credential = Credential { clientId, clientSecret }
                 , catchingTag
                 , targetUser
                 , tokenFilePath
                 , port
                 }

type HealthCheckAPI  = Get '[PlainText] T.Text
type ServerAPI = HealthCheckAPI :<|> TootReceiverAPI

healthCheckApi :: Proxy HealthCheckAPI
healthCheckApi = Proxy

serverApi :: Proxy ServerAPI
serverApi = Proxy

healthCheck :: Handler T.Text
healthCheck = do
    liftIO $ putStrLn "health checked !"
    return $ T.pack "healthy"


readToken :: FilePath -> IO Token
readToken file = do
    accessToken : refreshToken : _ <- T.lines <$> T.readFile file
    return $ Token { accessToken, refreshToken, expiresIn = Nothing }

writeToken :: FilePath -> Token -> IO ()
writeToken file token = do
    T.writeFile file $ T.unlines [accessToken token, refreshToken token]

server :: BS.ByteString -> (Event -> IO ()) -> Server ServerAPI
server secretKey cont = healthCheck :<|> tootReceiver secretKey cont


app :: BS.ByteString -> (Event -> IO ()) -> Application
app secretKey cont = serve serverApi (server secretKey cont)

logInfo :: T.Text -> IO ()
logInfo text = T.putStrLn $ "[INFO] " <> text

logError :: T.Text -> IO ()
logError text = T.putStrLn $ "[ERROR] " <> text

canReadWrite :: FilePath -> IO Bool
canReadWrite file = fileAccess file True True False

checkTokenFile :: FilePath -> IO ()
checkTokenFile tokenFile = do
    exists <- fileExist tokenFile
    unless exists $ do
        logError "Token file does not exist."
        exitFailure

    has_proper_permission <- canReadWrite tokenFile
    unless has_proper_permission $ do
        logError "Token file does not have proper permission (the file must have read and write permission)."
        exitFailure

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering

    env <- loadEnv

    checkTokenFile (tokenFilePath env)

    initToken <- readToken (tokenFilePath env)
    tokenRef <- tokenRefresher 5
                               (credential env)
                               initToken
                               (\token -> logInfo "token refreshed" >> writeToken (tokenFilePath env) token)

    tweetLock <- newMVar False

    logInfo "server started!"

    -- TODO: 未送信のポストをキューやらに格納させる
    withStdoutLogger $ \logger ->
        let settings = setPort (port env) $ setLogger logger defaultSettings
            cont event = do
                locked <- readMVar tweetLock

                if not locked then do
                    token <- readMVar tokenRef
                    res <- sendToot (targetUser env) (catchingTag env) token $ eventObject event
                    case res of
                        Right () -> return ()
                        Left (TooManyRequests Nothing) -> do
                            logInfo $ "reach rate limit. block until " <> T.pack (show (15 * 60 :: Int))
                            void $ forkIO $ bracket_ (swapMVar tweetLock True)
                                                     (swapMVar tweetLock False)
                                                     (threadDelay' $ 15 * 60)
                        Left (TooManyRequests (Just reset)) -> do
                            logInfo $ "reach rate limit. block until " <> T.pack (show reset)
                            currTime <- getCurrentTime
                            void $ forkIO $ bracket_ (swapMVar tweetLock True)
                                                     (swapMVar tweetLock False)
                                                     (threadDelay' $ diffUTCTime reset currTime)
                        Left PS.Unauthorized -> logInfo "token may be expired"
                        Left (Other msg) -> logInfo msg
                else
                    logInfo "now blocked"
        in
            runSettings settings (app (secretKey env) cont)

threadDelay' :: NominalDiffTime -> IO ()
threadDelay' diff = threadDelay $ P.truncate $ diff * 10 ^ (6 :: Int)

sendToot :: T.Text -> T.Text -> Token -> Toot -> IO (Either SendingPostError ())
sendToot targetUser catchingTag token toot
    | V.elem catchingTag . V.map T.toLower . V.map tagName $ tootTags toot
    , targetUser == acctUsername (tootAccount toot)
    , Just tweet' <- tootToTweet catchingTag toot
        = tweet token tweet'
    | otherwise = return $ Right ()

tootToTweet :: T.Text -> Toot -> Maybe TW.Post
tootToTweet catchingTag (Toot { tootContent = content, tootUrl = url }) =
    let content' = either (const Nothing) (Just . removeHashTag catchingTag) $ htmlToPlain $ content
    in
        case url of
            Nothing   -> mkPost <$> content'
            Just url' -> mkPost . addUrl url' . truncate (postMaxChars - postUrlChars - 2 - P.length ("[ｆｒｏｍ]:" :: String)) <$> content'

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
