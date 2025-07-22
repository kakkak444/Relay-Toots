{-  Copyright © 2025 kakkak444.

    This Source Code Form is subject to the terms of the Mozilla Public
    License, v. 2.0. If a copy of the MPL was not distributed with this
    file, You can obtain one at https://mozilla.org/MPL/2.0/.
-}

module TokenRefresher
    ( TokenRefreshError
    , tokenRefresher
    ) where

import Control.Concurrent
import Control.Exception
import Data.Maybe
import Data.Time.Clock
import Data.Twitter
import PostSender         (refresh)


data TokenRefreshError
    = TokenRefreshFailed
    | RetryCountExceeded
    deriving (Show)

instance Exception TokenRefreshError


defaultExpiresIn :: NominalDiffTime
defaultExpiresIn = 3600

defaultInterval :: NominalDiffTime
defaultInterval = 30

tokenRefresher :: Int -> Credential -> Token -> (Token -> IO ()) -> IO (MVar Token)
tokenRefresher maxRetry cred initToken fn = do
    newToken <- refresh cred initToken
    case newToken of
        Nothing -> throwIO TokenRefreshFailed
        Just newToken' -> do
            fn newToken'
            let expiresIn' = fromMaybe defaultExpiresIn $ expiresIn newToken'
            tokenRef <- newMVar newToken'
            _ <- forkIO $ threadDelay' expiresIn' >> go 0 tokenRef
            return tokenRef
  where
    go :: Int -> MVar Token -> IO a
    go !retried tokenRef
        | retried >= maxRetry = throwIO TokenRefreshFailed
        | otherwise = do
            oldToken <- readMVar tokenRef
            newToken <- modifyMVar tokenRef $ \token -> do
                newToken <- refresh cred token
                case newToken of
                    Nothing -> throwIO TokenRefreshFailed
                    Just !newToken' -> return (newToken', newToken')
            if oldToken == newToken then
                threadDelay' defaultInterval >> go (retried + 1) tokenRef
            else do
                fn newToken
                let expiresIn' = fromMaybe defaultExpiresIn $ expiresIn newToken
                threadDelay' expiresIn' >> go 0 tokenRef

threadDelay' :: NominalDiffTime -> IO ()
threadDelay' diff = threadDelay $ truncate $ diff * 10 ^ (6 :: Int)
