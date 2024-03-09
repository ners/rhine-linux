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
import FRP.Rhine.DBus (DBusClock (..))
import FRP.Rhine.I3 (I3Clock (..))
import I3IPC.Subscribe qualified as I3

data KitchenSinkEvent
    = DBusEvent (Tag DBusClock)
    | I3Event (Tag I3Clock)
    deriving (Show)

data KitchenSinkClock = KitchenSinkClock
    { dbusClock :: DBusClock
    , i3Clock :: I3Clock
    }

-- | A clock that never ticks.
data Never (tag :: Type) = Never

instance (MonadIO m) => Clock m (Never tag) where
    type Time (Never _) = UTCTime
    type Tag (Never tag) = tag
    initClock :: Never tag -> RunningClockInit m UTCTime tag
    initClock Never = do
        time <- liftIO getCurrentTime
        let clock = constM . forever . liftIO . threadDelay $ 10 ^ 9
        pure (clock, time)

instance GetClockProxy (Never tag)

initFallibleClock
    :: ( Clock m cl
       , MonadIO m
       , MonadCatch m
       , Time cl ~ Time (Never (Tag cl))
       )
    => cl
    -> RunningClockInit m (Time cl) (Tag cl)
initFallibleClock cl = catchAll (initClock cl) (const $ initClock Never)

instance Clock IO KitchenSinkClock where
    type Time KitchenSinkClock = UTCTime
    type Tag KitchenSinkClock = KitchenSinkEvent
    initClock
        :: KitchenSinkClock
        -> RunningClockInit IO (Time KitchenSinkClock) (Tag KitchenSinkClock)
    initClock ksc = do
        (dbusClock, time) <- initFallibleClock ksc.dbusClock
        (i3Clock, _) <- initFallibleClock ksc.i3Clock
        let clock =
                concatS $
                    scheduleList
                        [ dbusClock >>> arr (second DBusEvent)
                        , i3Clock >>> arr (second I3Event)
                        ]
                        >>> arr NonEmpty.toList
        pure (clock, time)

instance GetClockProxy KitchenSinkClock

main :: IO ()
main = flow $ (tagS >>> arrMCl print) @@ clock
  where
    dbusClock = DBusClock{matchRules = [DBus.matchAny]}
    i3Clock = I3Clock{subscriptions = [I3.Window, I3.Workspace]}
    clock = KitchenSinkClock{..}
