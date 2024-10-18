module FRP.Rhine.Evdev where

import Data.Time (getCurrentTime)
import Evdev
import FRP.Rhine
import System.Posix.ByteString (RawFilePath)
import Prelude

newtype EvdevClock = EvdevClock {device :: RawFilePath}

instance (MonadIO m) => Clock m EvdevClock where
    type Time EvdevClock = UTCTime
    type Tag EvdevClock = Event
    initClock
        :: EvdevClock -> RunningClockInit m (Time EvdevClock) (Tag EvdevClock)
    initClock EvdevClock{..} = liftIO do
        device' <- newDevice device
        let clock = constM $ liftIO do
                e <- nextEvent device'
                t <- getCurrentTime
                pure (t, e)
        (clock,) <$> getCurrentTime

instance GetClockProxy EvdevClock
