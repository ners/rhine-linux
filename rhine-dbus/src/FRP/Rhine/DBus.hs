module FRP.Rhine.DBus where

import Control.Concurrent.STM (atomically, newTQueueIO, readTQueue, writeTQueue)
import DBus (Signal)
import DBus.Client (MatchRule, addMatch, connectSession)
import Data.Foldable (for_)
import Data.Time (getCurrentTime)
import FRP.Rhine
import Prelude

newtype DBusClock = DBusClock {matchRules :: [MatchRule]}

instance (MonadIO m) => Clock m DBusClock where
    type Time DBusClock = UTCTime
    type Tag DBusClock = Signal
    initClock :: DBusClock -> RunningClockInit m (Time DBusClock) (Tag DBusClock)
    initClock DBusClock{..} = liftIO do
        client <- connectSession
        events <- newTQueueIO
        for_ matchRules $ \matchRule -> addMatch client matchRule $ atomically . writeTQueue events
        let
            clock :: MSF m a (Time DBusClock, Tag DBusClock)
            clock = constM $ liftIO do
                e <- atomically . readTQueue $ events
                t <- getCurrentTime
                pure (t, e)
        (clock,) <$> getCurrentTime

instance GetClockProxy DBusClock
