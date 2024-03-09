module FRP.Rhine.I3 where

import Control.Monad.Catch (MonadThrow)
import Data.Aeson (encode)
import Data.Functor (void)
import Data.Time.Clock (getCurrentTime)
import FRP.Rhine
import I3IPC (connecti3, receiveEvent)
import I3IPC.Event (Event)
import I3IPC.Message (MessageType (Subscribe), sendMsgPayload)
import I3IPC.Subscribe (Subscribe)
import Prelude

newtype I3Clock = I3Clock {subscriptions :: [Subscribe]}

instance (MonadIO m, MonadThrow m) => Clock m I3Clock where
    type Time I3Clock = UTCTime
    type Tag I3Clock = Event
    initClock :: I3Clock -> RunningClockInit m (Time I3Clock) (Tag I3Clock)
    initClock I3Clock{..} = do
        client <- connecti3
        void $ sendMsgPayload client Subscribe $ encode subscriptions
        let
            getEvent :: m (Tag I3Clock)
            getEvent = either (const getEvent) pure =<< receiveEvent client
            clock :: MSF m a (Time I3Clock, Tag I3Clock)
            clock = constM do
                e <- getEvent
                t <- liftIO getCurrentTime
                pure (t, e)
        (clock,) <$> liftIO getCurrentTime

instance GetClockProxy I3Clock
