module FRP.Rhine.X11 where

import Control.Monad (when)
import Data.Functor (void)
import Data.Maybe (fromMaybe, isJust)
import Data.Time.Clock (getCurrentTime)
import FRP.Rhine
import Graphics.X11
    ( allocaSetWindowAttributes
    , cWEventMask
    , propertyChangeMask
    , set_event_mask
    )
import Graphics.X11.Xlib (nextEvent, openDisplay)
import Graphics.X11.Xlib.Display (defaultRootWindow)
import Graphics.X11.Xlib.Event (allocaXEvent)
import Graphics.X11.Xlib.Extras
    ( Event
    , changeWindowAttributes
    , getEvent
    )
import Graphics.X11.Xrandr
    ( xrrQueryExtension
    , xrrUpdateConfiguration
    )
import Prelude

newtype X11Clock = X11Clock {displayName :: Maybe String}

instance (MonadIO m) => Clock m X11Clock where
    type Time X11Clock = UTCTime
    type Tag X11Clock = Event
    initClock :: X11Clock -> RunningClockInit m (Time X11Clock) (Tag X11Clock)
    initClock X11Clock{..} = liftIO do
        display <- openDisplay $ fromMaybe "" displayName
        let root = defaultRootWindow display
        allocaSetWindowAttributes $ \setWindowAttrsPtr -> do
            set_event_mask setWindowAttrsPtr propertyChangeMask
            changeWindowAttributes display root cWEventMask setWindowAttrsPtr
        let clock = constM . liftIO . allocaXEvent $ \eventPtr -> do
                rrData <- xrrQueryExtension display
                let rrUpdate = when (isJust rrData) . void . xrrUpdateConfiguration
                nextEvent display eventPtr
                rrUpdate eventPtr
                e <- getEvent eventPtr
                t <- getCurrentTime
                pure (t, e)
        (clock,) <$> getCurrentTime

instance GetClockProxy X11Clock
