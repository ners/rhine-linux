{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Codec.Picture (Image(..), PixelRGB8)
import Control.Monad.Catch (MonadCatch, catchAll)
import DBus.Client qualified as DBus
import Data.Functor ((<&>))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Vector.Storable qualified as Vector
import Data.Void (absurd)
import FRP.Rhine
import FRP.Rhine.DBus (DBusClock (..))
import FRP.Rhine.Evdev (EvdevClock (..))
import FRP.Rhine.I3 (I3Clock (..))
import FRP.Rhine.INotify (INotifyClock (..))
import FRP.Rhine.UDev (UDevClock (..), UDevFilter (..))
import FRP.Rhine.V4l2 (V4l2Clock (..))
import Graphics.V4L2 qualified as V4L2
import I3IPC.Subscribe qualified as I3
import System.IO (hPutStrLn, stderr)
import System.UDev qualified as UDev
import System.UDev.Monitor (SourceId (..))
import Prelude

data KitchenSinkEvent
    = DBusTag (Tag DBusClock)
    | EvdevTag (Tag EvdevClock)
    | I3Tag (Tag I3Clock)
    | INotifyTag (Tag INotifyClock)
    | UDevTag (Tag UDevClock)
    | V4l2Tag (Tag V4l2Clock)
    deriving stock (Show)

instance Show UDev.Device where
    show dev =
        mconcat
            [ "Device {"
            , "devNum=" <> show (UDev.getDevnum dev)
            , ", subsystem=" <> show (UDev.getSubsystem dev)
            , ", devtype=" <> show (UDev.getDevtype dev)
            , ", syspath=" <> show (UDev.getSyspath dev)
            , ", sysname=" <> show (UDev.getSysname dev)
            , ", sysnum=" <> show (UDev.getSysnum dev)
            , ", devnode=" <> show (UDev.getDevnode dev)
            , ", action=" <> show (UDev.getAction dev)
            , "}"
            ]

instance Show (Image PixelRGB8) where
    show Image{..} =
        mconcat
            [ "Image {"
            , "imageWidth=" <> show imageWidth
            , ", imageHeight=" <> show imageHeight
            , ", imageData=Vector[" <> show (Vector.length imageData) <> "]"
            , "}"
            ]

data KitchenSinkClock = KitchenSinkClock
    { dbusClock :: DBusClock
    , evdevClock :: EvdevClock
    , i3Clock :: I3Clock
    , inotifyClock :: INotifyClock
    , udevClock :: UDevClock
    , v4l2Clock :: V4l2Clock
    }
    deriving stock (Show)

deriving stock instance Show DBusClock

deriving stock instance Show EvdevClock

deriving stock instance Show I3Clock

deriving stock instance Show INotifyClock

deriving stock instance Show UDevClock

deriving stock instance Show UDevFilter

deriving stock instance Show V4l2Clock

(<@@)
    :: (Functor f, Arrow a)
    => f (a b1 (d1, b2), d2)
    -> (b2 -> c)
    -> f (a b1 (d1, c), d2)
cl <@@ f = cl <&> first (>>^ second f)

initFallibleClock
    :: ( Clock m cl
       , Show cl
       , MonadIO m
       , MonadCatch m
       , Time cl ~ Time Never
       )
    => cl
    -> RunningClockInit m (Time cl) (Tag cl)
initFallibleClock cl = catchAll (initClock cl) $ \e -> do
    liftIO $ hPutStrLn stderr $ "initFallibleClock " <> show cl <> ": " <> show e
    initClock Never <@@ absurd

instance Clock IO KitchenSinkClock where
    type Time KitchenSinkClock = UTCTime
    type Tag KitchenSinkClock = KitchenSinkEvent
    initClock
        :: KitchenSinkClock
        -> RunningClockInit IO (Time KitchenSinkClock) (Tag KitchenSinkClock)
    initClock KitchenSinkClock{..} = do
        clocks <-
            sequence
                [ initFallibleClock dbusClock <@@ DBusTag
                , initFallibleClock evdevClock <@@ EvdevTag
                , initFallibleClock i3Clock <@@ I3Tag
                , initFallibleClock inotifyClock <@@ INotifyTag
                , initFallibleClock udevClock <@@ UDevTag
                , initFallibleClock v4l2Clock <@@ V4l2Tag
                ]
        let clock =
                concatS $
                    scheduleList (fst <$> clocks)
                        >>^ NonEmpty.toList
        let time = snd $ NonEmpty.head clocks
        pure (clock, time)

instance GetClockProxy KitchenSinkClock

main :: IO ()
main = flow $ (tagS >>> arrMCl print) @@ KitchenSinkClock{..}
  where
    dbusClock = DBusClock{matchRules = [DBus.matchAny]}
    evdevClock = EvdevClock{device = "/dev/input/mouse0"}
    i3Clock = I3Clock{subscriptions = [I3.Window, I3.Workspace]}
    inotifyClock = INotifyClock{paths = ["."]}
    udevClock = UDevClock{sourceId = UDevId, filters = []}
    v4l2Clock =
        V4l2Clock
            { webcam = "/dev/video0"
            , format =
                let w = 1280 :: Int
                    h = 720 :: Int
                    d = 3 :: Int
                 in V4L2.ImageFormat
                        { imageWidth = w
                        , imageHeight = h
                        , imagePixelFormat = V4L2.PixelRGB24
                        , imageField = V4L2.FieldNone
                        , imageBytesPerLine = w * d
                        , imageSize = w * h * d
                        , imageColorSpace = V4L2.ColorJPEG
                        }
            }
