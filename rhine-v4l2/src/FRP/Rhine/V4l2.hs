module FRP.Rhine.V4l2 where

import Codec.Picture (Image (Image), PixelRGB8)
import Data.Functor (void)
import Data.Time (getCurrentTime)
import Data.Vector.Storable qualified as Vector
import FRP.Rhine
import Foreign (peek, plusPtr)
import Graphics.V4L2
import Prelude

data V4l2Clock = V4l2Clock
    { webcam :: FilePath
    , format :: ImageFormat
    }

instance (MonadIO m) => Clock m V4l2Clock where
    type Time V4l2Clock = UTCTime
    type Tag V4l2Clock = Image PixelRGB8
    initClock :: V4l2Clock -> RunningClockInit m (Time V4l2Clock) (Tag V4l2Clock)
    initClock V4l2Clock{..} = liftIO do
        device <- liftIO $ openDevice webcam
        void . liftIO $ setFormat device Capture format
        let
            clock :: MSF m a (Time V4l2Clock, Tag V4l2Clock)
            clock = constM $ liftIO do
                frame <- withFrame device format $ \ptr _ ->
                    let w = imageWidth format
                        h = imageHeight format
                     in Image w h <$> Vector.generateM (imageSize format) (peek . plusPtr ptr)
                time <- getCurrentTime
                pure (time, frame)
        (clock,) <$> getCurrentTime

instance GetClockProxy V4l2Clock
