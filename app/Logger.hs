{-  Copyright © 2025 kakkak444.

    This Source Code Form is subject to the terms of the Mozilla Public
    License, v. 2.0. If a copy of the MPL was not distributed with this
    file, You can obtain one at https://mozilla.org/MPL/2.0/.
-}

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}

module Logger
    ( LogLevel(..)
    , LogEvent(..)
    , LogChan

    , LoggingT(..)
    , HasLogger(..)

    , hoistLogging

    , defaultLogCapacity
    , runLoggingT
    , runStdoutLoggerT

    , writeLogChan
    , logWithLevel
    , logWithLevelS
    , logWithLevelN
    , logWithLevelNS
    , logTrace  , logDebug  , logInfo  , logWarn  , logError
    , logTraceS , logDebugS , logInfoS , logWarnS , logErrorS
    , logTraceN , logDebugN , logInfoN , logWarnN , logErrorN
    , logTraceNS, logDebugNS, logInfoNS, logWarnNS, logErrorNS
    ) where


import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TBChan
import Control.Exception
import Control.Monad
import Control.Monad.Reader
import Data.Ix                                      (Ix)
import Data.Time.Clock
import Data.Time.Format.ISO8601                     (iso8601Show)
import Data.Text                     qualified as T
import Data.Text.IO.Utf8             qualified as T
import System.IO                                    (stdout, hSetBuffering, BufferMode(..))
import UnliftIO                                     (MonadUnliftIO)


-- |
-- >>> minBound :: LogLevel
-- TRACE
-- >>> maxBound :: LogLevel
-- ERROR
data LogLevel
    = TRACE
    | DEBUG
    | INFO
    | WARN
    | ERROR
    deriving (Show, Eq, Ord, Ix, Bounded)

data LogEvent = LogEvent LogLevel UTCTime T.Text
    deriving (Show)

newtype LogChan = LogChan (TBChan LogEvent)

newtype LoggingT m a = LoggingT { unLoggingT :: ReaderT LogChan m a }
    deriving newtype ( Functor, Applicative, Monad, MonadTrans
                     , MonadIO
                     , MonadUnliftIO
                     , MonadFail
                     , MonadReader LogChan
                     )

class (Monad m) => HasLogger m where
    askLogChan :: m LogChan

instance (Monad m) => HasLogger (LoggingT m) where
    askLogChan = ask
instance (HasLogger m) => HasLogger (ReaderT e m) where
    askLogChan = askLogChan

hoistLogging :: (forall x. m x -> n x) -> LoggingT m a -> LoggingT n a
hoistLogging f (LoggingT m) = LoggingT $ ReaderT $ \e -> f $ runReaderT m e

loggerThread :: LogLevel -> LogChan -> IO a
loggerThread logLevel (LogChan chan) = forever $
    handle (\(e :: IOException) -> print e) $
    do
        event@(LogEvent level _ _) <- atomically $ readTBChan chan
        unless (level < logLevel) $
            T.putStrLn $ toText event

toText :: LogEvent -> T.Text
toText (LogEvent level time content) = T.concat ["[", T.show level, "] [", T.pack $ iso8601Show time, "] ", content]

defaultLogCapacity :: Int
defaultLogCapacity = 256

runStdoutLoggerT :: (MonadIO m) => LogLevel -> LoggingT m a -> m a
runStdoutLoggerT logLevel (LoggingT m) = do
    liftIO $ hSetBuffering stdout NoBuffering
    chan <- liftIO $ LogChan <$> newTBChanIO defaultLogCapacity
    _ <- liftIO $ forkIO $ loggerThread logLevel chan
    runReaderT m chan

runLoggingT :: LogChan -> LoggingT m a -> m a
runLoggingT chan = flip runReaderT chan . unLoggingT

writeLogChan :: (HasLogger m, MonadIO m) => LogEvent -> m ()
writeLogChan event = do
    LogChan chan <- askLogChan
    liftIO $ void $ forkIO $ atomically $ writeTBChan chan event

logWithLevel :: (HasLogger m, MonadIO m) => LogLevel -> T.Text -> m ()
logWithLevel level content = do
    currTime <- liftIO $ getCurrentTime
    writeLogChan $ LogEvent level currTime content

logWithLevelS :: (HasLogger m, MonadIO m, Show a) => LogLevel -> a -> m ()
logWithLevelS level content = do
    currTime <- liftIO $ getCurrentTime
    writeLogChan $ LogEvent level currTime $ T.show content

-- | log with name
logWithLevelN :: (HasLogger m, MonadIO m) => LogLevel -> T.Text -> T.Text -> m ()
logWithLevelN level name content = logWithLevel level $ T.concat [name, ": ", content]

logWithLevelNS :: (HasLogger m, MonadIO m, Show a) => LogLevel -> T.Text -> a -> m ()
logWithLevelNS level name content = logWithLevel level $ T.concat [name, ": ", T.show content]

logTrace, logDebug, logInfo, logWarn, logError :: (HasLogger m, MonadIO m) => T.Text -> m ()
logTrace = logWithLevel TRACE
logDebug = logWithLevel DEBUG
logInfo  = logWithLevel INFO
logWarn  = logWithLevel WARN
logError = logWithLevel ERROR

logTraceS, logDebugS, logInfoS, logWarnS, logErrorS :: (HasLogger m, MonadIO m, Show a) => a -> m ()
logTraceS = logWithLevelS TRACE
logDebugS = logWithLevelS DEBUG
logInfoS  = logWithLevelS INFO
logWarnS  = logWithLevelS WARN
logErrorS = logWithLevelS ERROR

logTraceN, logDebugN, logInfoN, logWarnN, logErrorN :: (HasLogger m, MonadIO m) => T.Text -> T.Text -> m ()
logTraceN = logWithLevelN TRACE
logDebugN = logWithLevelN DEBUG
logInfoN  = logWithLevelN INFO
logWarnN  = logWithLevelN WARN
logErrorN = logWithLevelN ERROR

logTraceNS, logDebugNS, logInfoNS, logWarnNS, logErrorNS :: (HasLogger m, MonadIO m, Show a) => T.Text -> a -> m ()
logTraceNS = logWithLevelNS TRACE
logDebugNS = logWithLevelNS DEBUG
logInfoNS  = logWithLevelNS INFO
logWarnNS  = logWithLevelNS WARN
logErrorNS = logWithLevelNS ERROR
