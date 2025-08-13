{-  Copyright © 2025 kakkak444.

    This Source Code Form is subject to the terms of the Mozilla Public
    License, v. 2.0. If a copy of the MPL was not distributed with this
    file, You can obtain one at https://mozilla.org/MPL/2.0/.
-}

{-# LANGUAGE DerivingStrategies #-}

module App
    ( module Env
    , module Logger

    , AppT
    , hoistApp
    , runApp

    , HasSecretKey(..)
    , HasCredential(..)
    , HasTargetUser(..)
    , HasCatchingTag(..)
    , HasTokenFilePath(..)
    , HasPort(..)
    , HasTwitterToken(..)
    , HasTweetLock(..)
    ) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Reader
import Data.ByteString      qualified as BS
import Data.Maybe
import Data.Text            qualified as T
import Data.Time.Clock
import Data.Twitter
import Env
import Logger
import UnliftIO                             (MonadUnliftIO)


data AppConfig = AppConfig
    { appEnv           :: Env
    , appTwitterToken  :: MVar Token
    , appTweetLocker   :: MVar () -- ^ empty => locked, full => not locked
    }

newtype AppT m a = AppT { unAppT :: ReaderT AppConfig (LoggingT m) a }
    deriving newtype ( Functor, Applicative, Monad
                     , MonadFail
                     , MonadReader AppConfig
                     , MonadIO, MonadUnliftIO
                     , HasSecretKey
                     , HasCredential
                     , HasTargetUser
                     , HasCatchingTag
                     , HasTokenFilePath
                     , HasPort
                     , HasTwitterToken
                     , HasTweetLock
                     , HasLogger
                     )


runApp :: (MonadIO m) => Env -> MVar Token -> AppT m a -> LoggingT m a
runApp env refreshingToken app = do
    locker   <- liftIO $ newMVar ()
    let config = AppConfig { appEnv = env, appTwitterToken = refreshingToken, appTweetLocker = locker }
    runReaderT (unAppT app) config

hoistApp :: (forall a. m a -> n a) -> AppT m x -> AppT n x
hoistApp f (AppT app) = AppT $ ReaderT $ \env -> hoistLogging f $ runReaderT app env


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

class (Monad m) => HasTwitterToken m where
    askTwitterToken :: m Token

class (Monad m) => HasTweetLock m where
    tweetIsLocked  :: m Bool
    -- | 少なくとも`引数`の間はロックされていることを保証する（それ以上ロックされる可能性がある）
    lockTweet      :: NominalDiffTime -> m ()


instance (Monad m) => HasSecretKey (ReaderT Env m) where
    askSecretKey = asks envSecretKey
instance (Monad m) => HasCredential (ReaderT Env m) where
    askCredential = asks envCredential
instance (Monad m) => HasTargetUser (ReaderT Env m) where
    askTargetUser = asks envTargetUser
instance (Monad m) => HasCatchingTag (ReaderT Env m) where
    askCatchingTag = asks envCatchingTag
instance (Monad m) => HasTokenFilePath (ReaderT Env m) where
    askTokenFilePath = asks envTokenFilePath
instance (Monad m) => HasPort (ReaderT Env m) where
    askPort = asks envPort

instance (Monad m) => HasSecretKey (ReaderT AppConfig m) where
    askSecretKey = hoist askSecretKey
instance (Monad m) => HasCredential (ReaderT AppConfig m) where
    askCredential = hoist askCredential
instance (Monad m) => HasTargetUser (ReaderT AppConfig m) where
    askTargetUser = hoist askTargetUser
instance (Monad m) => HasCatchingTag (ReaderT AppConfig m) where
    askCatchingTag = hoist askCatchingTag
instance (Monad m) => HasTokenFilePath (ReaderT AppConfig m) where
    askTokenFilePath = hoist askTokenFilePath
instance (Monad m) => HasPort (ReaderT AppConfig m) where
    askPort = hoist askPort
instance (MonadIO m) => HasTwitterToken (ReaderT AppConfig m) where
    askTwitterToken = asks appTwitterToken >>= liftIO . readMVar
instance (MonadIO m) => HasTweetLock (ReaderT AppConfig m) where
    tweetIsLocked = isNothing <$> (asks appTweetLocker >>= liftIO . tryReadMVar)
    lockTweet t = do
        locker <- asks appTweetLocker
        liftIO $ void $ forkIO $ bracket_ (takeMVar locker)
                                          (putMVar locker ())
                                          (threadDelay' t)


threadDelay' :: NominalDiffTime -> IO ()
threadDelay' diff = threadDelay $ truncate $ diff * 10 ^ (6 :: Int)

hoist :: ReaderT Env m a -> ReaderT AppConfig m a
hoist = withReaderT appEnv
