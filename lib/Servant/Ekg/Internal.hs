{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Servant.Ekg.Internal where

import           Control.Exception
import           Control.Monad
import           Data.Hashable               (Hashable (..))
import qualified Data.HashMap.Strict         as H
import           Data.Monoid
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T
import           Data.Time.Clock
import           GHC.Generics                (Generic)
import           Network.HTTP.Types          (Method, Status (..))
import           Network.Wai                 (Middleware, responseStatus)
import           System.Metrics
import qualified System.Metrics.Counter      as Counter
import qualified System.Metrics.Distribution as Distribution
import qualified System.Metrics.Gauge        as Gauge

data Meters = Meters
    { metersInflight :: Gauge.Gauge
    , metersC2XX     :: Counter.Counter
    , metersC4XX     :: Counter.Counter
    , metersC5XX     :: Counter.Counter
    , metersCXXX     :: Counter.Counter
    , metersTime     :: Distribution.Distribution
    }

data APIEndpoint = APIEndpoint {
    pathSegments :: [Text],
    method       :: Method
} deriving (Eq, Hashable, Show, Generic)

gaugeInflight :: Gauge.Gauge -> Middleware
gaugeInflight inflight application request respond =
    bracket_ (Gauge.inc inflight)
             (Gauge.dec inflight)
             (application request respond)

-- | Count responses with 2XX, 4XX, 5XX, and XXX response codes.
countResponseCodes
    :: (Counter.Counter, Counter.Counter, Counter.Counter, Counter.Counter)
    -> Middleware
countResponseCodes (c2XX, c4XX, c5XX, cXXX) application request respond =
    application request respond'
  where
    respond' res = count (responseStatus res) >> respond res
    count Status{statusCode = sc }
        | 200 <= sc && sc < 300 = Counter.inc c2XX
        | 400 <= sc && sc < 500 = Counter.inc c4XX
        | 500 <= sc && sc < 600 = Counter.inc c5XX
        | otherwise             = Counter.inc cXXX

responseTimeDistribution :: Distribution.Distribution -> Middleware
responseTimeDistribution dist application request respond =
    bracket getCurrentTime stop $ const $ application request respond
  where
    stop t1 = do
        t2 <- getCurrentTime
        let dt = diffUTCTime t2 t1
        Distribution.add dist $ fromRational $ (*1000) $ toRational dt

initializeMeters :: Store -> APIEndpoint -> IO Meters
initializeMeters store APIEndpoint{..} = do
    metersInflight <- createGauge        (prefix <> "in_flight") store
    metersC2XX     <- createCounter      (prefix <> "responses.2XX") store
    metersC4XX     <- createCounter      (prefix <> "responses.4XX") store
    metersC5XX     <- createCounter      (prefix <> "responses.5XX") store
    metersCXXX     <- createCounter      (prefix <> "responses.XXX") store
    metersTime     <- createDistribution (prefix <> "time_ms") store

    return Meters{..}

    where
        prefix = "servant.path." <> path <> "."
        path   = T.intercalate "." $ pathSegments <> [T.decodeUtf8 method]

initializeMetersTable :: Store -> [APIEndpoint] -> IO (H.HashMap APIEndpoint Meters)
initializeMetersTable store endpoints = do
    meters <- mapM (initializeMeters store) endpoints

    return $ H.fromList (zip endpoints meters)
