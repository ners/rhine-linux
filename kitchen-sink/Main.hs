{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (catch)
import Control.Monad (forever)
import Control.Monad.Catch (MonadCatch, catchAll)
import DBus.Client qualified as DBus
import Data.Kind (Type)
import Data.List.NonEmpty qualified as NonEmpty
import Data.MonadicStreamFunction.Async (concatS)
import Data.Time (UTCTime (UTCTime), getCurrentTime)
import FRP.Rhine
import FRP.Rhine.Clock.Realtime.Never (Never)
import FRP.Rhine.DBus (DBusClock (..))
import FRP.Rhine.I3 (I3Clock (..))
import FRP.Rhine.UDev (UDevClock (..))
import I3IPC.Subscribe qualified as I3
import System.IO (hPutStrLn, stderr)
import System.UDev
    ( Device (..)
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

data KitchenSinkEvent
    = DBusTag (Tag DBusClock)
    | I3Tag (Tag I3Clock)
    | UDevTag (Tag UDevClock)
    deriving (Show)

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
    , udevClock :: UDevClock
    }

initFallibleClock
    :: ( Clock m cl
       , MonadIO m
       , MonadCatch m
       , Time cl ~ Time (Never (Tag cl))
       )
    => cl
    -> RunningClockInit m (Time cl) (Tag cl)
initFallibleClock cl = catchAll (initClock cl) $ \e -> do
    liftIO $ hPutStrLn stderr $ "initFallibleClock: " <> show e
    initClock Never

instance Clock IO KitchenSinkClock where
    type Time KitchenSinkClock = UTCTime
    type Tag KitchenSinkClock = KitchenSinkEvent
    initClock
        :: KitchenSinkClock
        -> RunningClockInit IO (Time KitchenSinkClock) (Tag KitchenSinkClock)
    initClock ksc = do
        (dbusClock, time) <- initFallibleClock ksc.dbusClock
        (i3Clock, _) <- initFallibleClock ksc.i3Clock
        (udevClock, _) <- initFallibleClock ksc.udevClock
        let clock =
                concatS $
                    scheduleList
                        [ dbusClock >>> arr (second DBusTag)
                        , i3Clock >>> arr (second I3Tag)
                        , udevClock >>> arr (second UDevTag)
                        ]
                        >>> arr NonEmpty.toList
        pure (clock, time)

instance GetClockProxy KitchenSinkClock

main :: IO ()
main = flow $ (tagS >>> arrMCl print) @@ KitchenSinkClock{..}
  where
    dbusClock = DBusClock{matchRules = [DBus.matchAny]}
    i3Clock = I3Clock{subscriptions = [I3.Window, I3.Workspace]}
    udevClock = UDevClock{sourceId = UDevId, filters = []}
