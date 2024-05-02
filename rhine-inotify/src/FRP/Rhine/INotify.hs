module FRP.Rhine.INotify where

import Control.Concurrent.STM (atomically, newTQueueIO, readTQueue, writeTQueue)
import Data.Foldable (for_)
import Data.Time (getCurrentTime)
import FRP.Rhine
import System.INotify (Event, EventVariety (AllEvents), addWatch, initINotify)
import System.Posix.ByteString (RawFilePath)
import Prelude

newtype INotifyClock = INotifyClock {paths :: [RawFilePath]}

instance (MonadIO m) => Clock m INotifyClock where
    type Time INotifyClock = UTCTime
    type Tag INotifyClock = Event
    initClock
        :: INotifyClock -> RunningClockInit m (Time INotifyClock) (Tag INotifyClock)
    initClock INotifyClock{..} = liftIO do
        inotify <- initINotify
        events <- newTQueueIO
        for_ paths $ \path -> addWatch inotify [AllEvents] path $ atomically . writeTQueue events
        let clock = constM $ liftIO do
                e <- atomically . readTQueue $ events
                t <- getCurrentTime
                pure (t, e)
        (clock,) <$> getCurrentTime

instance GetClockProxy INotifyClock
