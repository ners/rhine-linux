module FRP.Rhine.I3 where

import Control.Monad.Catch (MonadThrow)
import Data.Aeson (encode)
import Data.Functor (void, (<&>))
import Data.Time.Clock (getCurrentTime)
import FRP.Rhine
import I3IPC (connecti3, receiveEvent)
import I3IPC.Event (Event)
import I3IPC.Message (MessageType(Subscribe), sendMsgPayload)
import I3IPC.Subscribe (Subscribe)
import Prelude

newtype I3Clock = I3Clock {subscriptions :: [Subscribe]}

instance (MonadIO m, MonadThrow m) => Clock m I3Clock where
    type Time I3Clock = UTCTime
    type Tag I3Clock = Event
    initClock :: I3Clock -> RunningClockInit m (Time I3Clock) (Tag I3Clock)
    initClock I3Clock{..} = do
        soc <- connecti3
        void $ sendMsgPayload soc Subscribe (encode subscriptions)
        let
            getEvent :: m (Tag I3Clock)
            getEvent =
                receiveEvent soc >>= \case
                    Right e -> pure e
                    Left _ -> getEvent
            tagEvent :: Tag I3Clock -> m (Time I3Clock, Tag I3Clock)
            tagEvent e = liftIO getCurrentTime <&> (,e)
            clock :: MSF m a (Time I3Clock, Tag I3Clock)
            clock = constM $ getEvent >>= tagEvent
        (clock,) <$> liftIO getCurrentTime

instance GetClockProxy I3Clock
