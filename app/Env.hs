{-  Copyright © 2025 kakkak444.

    This Source Code Form is subject to the terms of the Mozilla Public
    License, v. 2.0. If a copy of the MPL was not distributed with this
    file, You can obtain one at https://mozilla.org/MPL/2.0/.
-}

module Env
    ( Env(..)
    , loadEnv
    ) where


import Control.Monad.IO.Class                  (MonadIO, liftIO)
import Data.ByteString        qualified as BS
import Data.ByteString.Char8  qualified as BS8
import Data.Text              qualified as T
import Data.Twitter
import System.Environment                      (getEnv)
import System.Environment.Blank                (getEnvDefault)


data Env = Env
    { envSecretKey     :: BS.ByteString
    , envCredential    :: Credential
    , envTargetUser    :: T.Text
    , envCatchingTag   :: T.Text
    , envTokenFilePath :: FilePath
    , envPort          :: Int
    }

loadEnv :: (MonadIO m) => m Env
loadEnv = liftIO $ do
    secretKey    <- BS8.pack <$> getEnv "WEBHOOK_SECRET_KEY"

    clientId     <- T.pack   <$> getEnv "X_API_CLIENT_ID"
    clientSecret <- T.pack   <$> getEnv "X_API_CLIENT_SECRET"

    catchingTag   <- T.toLower . T.pack <$> getEnv "RELAY_TAG"
    targetUser    <- T.pack <$> getEnv "TARGET"
    tokenFilePath <- getEnv "TOKEN_PATH"

    port <- readIO =<< getEnvDefault "PORT" "3000"

    return $ Env { envSecretKey     = secretKey
                 , envCredential    = Credential { clientId, clientSecret }
                 , envCatchingTag   = catchingTag
                 , envTargetUser    = targetUser
                 , envTokenFilePath = tokenFilePath
                 , envPort          = port
                 }
