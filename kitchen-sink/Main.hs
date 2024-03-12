{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Control.Monad.Catch (MonadCatch, catchAll)
import DBus.Client qualified as DBus
import Data.Functor ((<&>))
import Data.List.NonEmpty qualified as NonEmpty
import Data.MonadicStreamFunction.Async (concatS)
import Data.Void (absurd)
import FRP.Rhine
import FRP.Rhine.DBus (DBusClock (..))
import FRP.Rhine.I3 (I3Clock (..))
import FRP.Rhine.INotify (INotifyClock (..))
import FRP.Rhine.UDev (UDevClock (..))
import I3IPC.Subscribe qualified as I3
import System.IO (hPutStrLn, stderr)
import System.UDev
    ( Device
    , getAction
    , getDevnode
    , getDevnum
    , getDevtype
    , getSubsystem
    , getSysname
    , getSysnum
    , getSyspath
    )
import System.UDev.Monitor (SourceId (..))
import Prelude

data KitchenSinkEvent
    = DBusTag (Tag DBusClock)
    | I3Tag (Tag I3Clock)
    | INotifyTag (Tag INotifyClock)
    | UDevTag (Tag UDevClock)
    deriving stock (Show)

instance Show Device where
    show dev =
        mconcat
            [ "Device {"
            , "devNum=" <> show (getDevnum dev)
            , ", subsystem=" <> show (getSubsystem dev)
            , ", devtype=" <> show (getDevtype dev)
            , ", syspath=" <> show (getSyspath dev)
            , ", sysname=" <> show (getSysname dev)
            , ", sysnum=" <> show (getSysnum dev)
            , ", devnode=" <> show (getDevnode dev)
            , ", action=" <> show (getAction dev)
            , "}"
            ]

data KitchenSinkClock = KitchenSinkClock
    { dbusClock :: DBusClock
    , i3Clock :: I3Clock
    , inotifyClock :: INotifyClock
    , udevClock :: UDevClock
    }

foo :: (Arrow a) => (b1 -> c) -> (a b2 (d1, b1), d2) -> (a b2 (d1, c), d2)
foo f = first (>>^ second f)

initFallibleClock
    :: ( Clock m cl
       , MonadIO m
       , MonadCatch m
       , Time cl ~ Time Never
       )
    => cl
    -> RunningClockInit m (Time cl) (Tag cl)
initFallibleClock cl = catchAll (initClock cl) $ \e -> do
    liftIO $ hPutStrLn stderr $ "initFallibleClock: " <> show e
    initClock Never <&> foo absurd

instance Clock IO KitchenSinkClock where
    type Time KitchenSinkClock = UTCTime
    type Tag KitchenSinkClock = KitchenSinkEvent
    initClock
        :: KitchenSinkClock
        -> RunningClockInit IO (Time KitchenSinkClock) (Tag KitchenSinkClock)
    initClock KitchenSinkClock{..} = do
        clocks <-
            sequence
                [ initFallibleClock dbusClock <&> foo DBusTag
                , initFallibleClock i3Clock <&> foo I3Tag
                , initFallibleClock inotifyClock <&> foo INotifyTag
                , initFallibleClock udevClock <&> foo UDevTag
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
    i3Clock = I3Clock{subscriptions = [I3.Window, I3.Workspace]}
    inotifyClock = INotifyClock{paths = ["."]}
    udevClock = UDevClock{sourceId = UDevId, filters = []}
