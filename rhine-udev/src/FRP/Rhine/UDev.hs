{-# OPTIONS_GHC -Wno-partial-fields #-}

module FRP.Rhine.UDev where

import Data.ByteString (ByteString)
import Data.Foldable (for_)
import Data.Time (getCurrentTime)
import FRP.Rhine
import System.UDev
    ( Device
    , SourceId
    , enableReceiving
    , filterAddMatchSubsystemDevtype
    , filterAddMatchTag
    , newFromNetlink
    , newUDev
    , receiveDevice
    )
import Prelude

data UDevFilter
    = Subsystem
        { subsystem :: ByteString
        , deviceType :: Maybe ByteString
        }
    | Tag {tag :: ByteString}

data UDevClock = UDevClock
    { sourceId :: SourceId
    , filters :: [UDevFilter]
    }

instance (MonadIO m) => Clock m UDevClock where
    type Time UDevClock = UTCTime
    type Tag UDevClock = Device
    initClock :: UDevClock -> RunningClockInit m (Time UDevClock) (Tag UDevClock)
    initClock UDevClock{..} = liftIO do
        udev <- newUDev
        monitor <- newFromNetlink udev sourceId
        for_ filters \case
            Subsystem{..} -> filterAddMatchSubsystemDevtype monitor subsystem deviceType
            Tag{..} -> filterAddMatchTag monitor tag
        enableReceiving monitor
        let
            clock :: MSF m a (Time UDevClock, Tag UDevClock)
            clock = constM $ liftIO do
                d <- receiveDevice monitor
                t <- getCurrentTime
                pure (t, d)
        (clock,) <$> getCurrentTime

instance GetClockProxy UDevClock
