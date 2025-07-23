{-# LANGUAGE DerivingStrategies #-}

module Env
    ( HasSecretKey(..)
    , HasCredential(..)
    , HasTargetUser(..)
    , HasCatchingTag(..)
    , HasTokenFilePath(..)
    , HasPort(..)

    , AppT
    , Env(..)
    , hoistApp
    , loadEnv
    , runAppT
    ) where

import Control.Monad.Reader
import Data.ByteString        qualified as BS
import Data.ByteString.Char8  qualified as BS8
import Data.Text              qualified as T
import Data.Twitter
import System.Environment                      (getEnv)
import System.Environment.Blank                (getEnvDefault)


class (Monad m) => HasSecretKey m where
    askSecretKey :: m BS.ByteString

class (Monad m) => HasCredential m where
    askCredential :: m Credential

class (Monad m) => HasTargetUser m where
    askTargetUser :: m T.Text

class (Monad m) => HasCatchingTag m where
    askCatchingTag :: m T.Text

class (Monad m) => HasTokenFilePath m where
    askTokenFilePath :: m FilePath

class (Monad m) => HasPort m where
    askPort :: m Int


newtype AppT m a = AppT { unAppT :: ReaderT Env m a }
    deriving newtype ( Functor, Applicative, Monad, MonadTrans
                     , MonadReader Env
                     , MonadIO
                     )

data Env = Env
    { envSecretKey     :: BS.ByteString
    , envCredential    :: Credential
    , envTargetUser    :: T.Text
    , envCatchingTag   :: T.Text
    , envTokenFilePath :: FilePath
    , envPort          :: Int
    }

runAppT :: Env -> AppT m a -> m a
runAppT env = flip runReaderT env . unAppT

hoistApp :: (forall a. m a -> n a) -> AppT m x -> AppT n x
hoistApp f (AppT app) = AppT $ ReaderT $ \env -> f $ runReaderT app env

loadEnv :: IO Env
loadEnv = do
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

instance (Monad m) => HasSecretKey (AppT m) where
    askSecretKey = asks envSecretKey
instance (Monad m) => HasCredential (AppT m) where
    askCredential = asks envCredential
instance (Monad m) => HasTargetUser (AppT m) where
    askTargetUser = asks envTargetUser
instance (Monad m) => HasCatchingTag (AppT m) where
    askCatchingTag = asks envCatchingTag
instance (Monad m) => HasTokenFilePath (AppT m) where
    askTokenFilePath = asks envTokenFilePath
instance (Monad m) => HasPort (AppT m) where
    askPort = asks envPort
