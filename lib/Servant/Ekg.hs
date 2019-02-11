{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
module Servant.Ekg where

import           Control.Concurrent.MVar
import           Control.Exception
import           Control.Monad
import qualified Data.HashMap.Strict         as H
import           Data.Monoid
import           Data.Proxy
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T
import           Data.Time.Clock
import           GHC.TypeLits
import           Network.HTTP.Types          (Method, Status (..))
import           Network.Wai
import           Servant.API
import           System.Metrics
import qualified System.Metrics.Counter      as Counter
import qualified System.Metrics.Distribution as Distribution
import qualified System.Metrics.Gauge        as Gauge

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

data Meters = Meters
    { metersInflight :: Gauge.Gauge
    , metersC2XX     :: Counter.Counter
    , metersC4XX     :: Counter.Counter
    , metersC5XX     :: Counter.Counter
    , metersCXXX     :: Counter.Counter
    , metersTime     :: Distribution.Distribution
    }

monitorEndpoints
    :: HasEndpoint api
    => Proxy api
    -> Store
    -> MVar (H.HashMap Text Meters)
    -> Middleware
monitorEndpoints proxy store meters application request respond = do
    let path = case getEndpoint proxy request of
            Nothing -> "unknown"
            Just (ps,method) -> T.intercalate "." $ ps <> [T.decodeUtf8 method]
    Meters{..} <- modifyMVar meters $ \ms -> case H.lookup path ms of
        Nothing -> do
            let prefix = "servant.path." <> path <> "."
            metersInflight <- createGauge (prefix <> "in_flight") store
            metersC2XX <- createCounter (prefix <> "responses.2XX") store
            metersC4XX <- createCounter (prefix <> "responses.4XX") store
            metersC5XX <- createCounter (prefix <> "responses.5XX") store
            metersCXXX <- createCounter (prefix <> "responses.XXX") store
            metersTime <- createDistribution (prefix <> "time_ms") store
            let m = Meters{..}
            return (H.insert path m ms, m)
        Just m -> return (ms,m)
    let application' =
            responseTimeDistribution metersTime .
            countResponseCodes (metersC2XX, metersC4XX, metersC5XX, metersCXXX) .
            gaugeInflight metersInflight $
            application
    application' request respond

class HasEndpoint a where
    getEndpoint :: Proxy a -> Request -> Maybe ([Text], Method)

instance HasEndpoint EmptyAPI where
    getEndpoint _ _ = Nothing

instance (HasEndpoint (a :: *), HasEndpoint (b :: *)) => HasEndpoint (a :<|> b) where
    getEndpoint _ req =
        getEndpoint (Proxy :: Proxy a) req `mplus`
        getEndpoint (Proxy :: Proxy b) req

instance (KnownSymbol (path :: Symbol), HasEndpoint (sub :: *))
    => HasEndpoint (path :> sub) where
    getEndpoint _ req =
        case pathInfo req of
            p:ps | p == T.pack (symbolVal (Proxy :: Proxy path)) -> do
                (end, method) <- getEndpoint (Proxy :: Proxy sub) req{ pathInfo = ps }
                return (p:end, method)
            _ -> Nothing


instance (KnownSymbol (capture :: Symbol), HasEndpoint (sub :: *))
    => HasEndpoint (Capture' mods capture a :> sub) where
    getEndpoint _ req =
        case pathInfo req of
            _:ps -> do
                (end, method) <- getEndpoint (Proxy :: Proxy sub) req{ pathInfo = ps }
                let p = T.pack $ (':':) $ symbolVal (Proxy :: Proxy capture)
                return (p:end, method)
            _ -> Nothing

instance HasEndpoint (sub :: *) => HasEndpoint (Summary d :> sub) where
    getEndpoint _ = getEndpoint (Proxy :: Proxy sub)

instance HasEndpoint (sub :: *) => HasEndpoint (Description d :> sub) where
    getEndpoint _ = getEndpoint (Proxy :: Proxy sub)

instance HasEndpoint (sub :: *) => HasEndpoint (Header' mods h a :> sub) where
    getEndpoint _ = getEndpoint (Proxy :: Proxy sub)

instance HasEndpoint (sub :: *) => HasEndpoint (QueryParam' mods (h :: Symbol) a :> sub) where
    getEndpoint _ = getEndpoint (Proxy :: Proxy sub)

instance HasEndpoint (sub :: *) => HasEndpoint (QueryParams (h :: Symbol) a :> sub) where
    getEndpoint _ = getEndpoint (Proxy :: Proxy sub)

instance HasEndpoint (sub :: *) => HasEndpoint (QueryFlag h :> sub) where
    getEndpoint _ = getEndpoint (Proxy :: Proxy sub)

instance HasEndpoint (sub :: *) => HasEndpoint (ReqBody' mods cts a :> sub) where
    getEndpoint _ = getEndpoint (Proxy :: Proxy sub)

#if MIN_VERSION_servant(0,15,0)
instance HasEndpoint (sub :: *) => HasEndpoint (StreamBody' mods framing ct a :> sub) where
    getEndpoint _ = getEndpoint (Proxy :: Proxy sub)
#endif

instance HasEndpoint (sub :: *) => HasEndpoint (RemoteHost :> sub) where
    getEndpoint _ = getEndpoint (Proxy :: Proxy sub)

instance HasEndpoint (sub :: *) => HasEndpoint (IsSecure :> sub) where
    getEndpoint _ = getEndpoint (Proxy :: Proxy sub)

instance HasEndpoint (sub :: *) => HasEndpoint (HttpVersion :> sub) where
    getEndpoint _ = getEndpoint (Proxy :: Proxy sub)

instance HasEndpoint (sub :: *) => HasEndpoint (Vault :> sub) where
    getEndpoint _ = getEndpoint (Proxy :: Proxy sub)

instance HasEndpoint (sub :: *) => HasEndpoint (WithNamedContext x y sub) where
    getEndpoint _ = getEndpoint (Proxy :: Proxy sub)

instance ReflectMethod method => HasEndpoint (Verb method status cts a) where
    getEndpoint _ req = case pathInfo req of
        [] | requestMethod req == method -> Just ([], method)
        _                                -> Nothing
      where method = reflectMethod (Proxy :: Proxy method)

instance ReflectMethod method => HasEndpoint (Stream method status framing ct a) where
    getEndpoint _ req = case pathInfo req of
        [] | requestMethod req == method -> Just ([], method)
        _                                -> Nothing
      where method = reflectMethod (Proxy :: Proxy method)

instance HasEndpoint Raw where
    getEndpoint _ _ = Just ([],"RAW")

instance HasEndpoint (sub :: *) => HasEndpoint (CaptureAll (h :: Symbol) a :> sub) where
    getEndpoint _ = getEndpoint (Proxy :: Proxy sub)
