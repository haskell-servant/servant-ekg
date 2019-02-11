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

module Servant.Ekg (
    HasEndpoint(..),
    APIEndpoint(..),
    monitorEndpoints
) where

import           Control.Exception
import           Control.Monad
import           Data.Hashable               (Hashable (..))
import qualified Data.HashMap.Strict         as H
import           Data.Monoid
import           Data.Proxy
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T
import           GHC.TypeLits
import           Network.HTTP.Types          (Method)
import           Network.Wai
import           Servant.API
import           Servant.Ekg.Internal
import           System.Metrics
import qualified System.Metrics.Counter      as Counter
import qualified System.Metrics.Distribution as Distribution
import qualified System.Metrics.Gauge        as Gauge


monitorEndpoints :: HasEndpoint api => Proxy api -> Store -> IO Middleware
monitorEndpoints proxy store = do
    meters <- initializeMetersTable store (enumerateEndpoints proxy)
    return (monitorEndpoints' meters)

    where
        monitorEndpoints' :: H.HashMap APIEndpoint Meters -> Middleware
        monitorEndpoints' meters application request respond =
            case getEndpoint proxy request >>= \ep -> H.lookup ep meters of
                Nothing ->
                    application request respond
                Just meters ->
                    updateCounters meters application request respond

            where
                updateCounters Meters{..} =
                      responseTimeDistribution metersTime
                    . countResponseCodes (metersC2XX, metersC4XX, metersC5XX, metersCXXX)
                    . gaugeInflight metersInflight


class HasEndpoint a where
    getEndpoint        :: Proxy a -> Request -> Maybe APIEndpoint
    enumerateEndpoints :: Proxy a -> [APIEndpoint]

instance HasEndpoint EmptyAPI where
    getEndpoint      _ _ = Nothing
    enumerateEndpoints _ = []

instance (HasEndpoint (a :: *), HasEndpoint (b :: *)) => HasEndpoint (a :<|> b) where
    getEndpoint _ req =
                getEndpoint (Proxy :: Proxy a) req
        `mplus` getEndpoint (Proxy :: Proxy b) req

    enumerateEndpoints _ =
           enumerateEndpoints (Proxy :: Proxy a)
        <> enumerateEndpoints (Proxy :: Proxy b)

instance (KnownSymbol (path :: Symbol), HasEndpoint (sub :: *))
    => HasEndpoint (path :> sub) where
    getEndpoint _ req =
        case pathInfo req of
            p:ps | p == T.pack (symbolVal (Proxy :: Proxy path)) -> do
                APIEndpoint{..} <- getEndpoint (Proxy :: Proxy sub) req{ pathInfo = ps }
                return (APIEndpoint (p:pathSegments) method)
            _ -> Nothing

    enumerateEndpoints _ =
        let endpoints               = enumerateEndpoints (Proxy :: Proxy sub)
            currentSegment          = T.pack $ symbolVal (Proxy :: Proxy path)
            qualify APIEndpoint{..} = APIEndpoint (currentSegment : pathSegments) method
        in
            map qualify endpoints

instance (KnownSymbol (capture :: Symbol), HasEndpoint (sub :: *))
    => HasEndpoint (Capture' mods capture a :> sub) where
    getEndpoint _ req =
        case pathInfo req of
            _:ps -> do
                APIEndpoint{..} <- getEndpoint (Proxy :: Proxy sub) req{ pathInfo = ps }
                let p = T.pack $ (':':) $ symbolVal (Proxy :: Proxy capture)
                return (APIEndpoint (p:pathSegments) method)
            _ -> Nothing
    enumerateEndpoints _ =
        let endpoints               = enumerateEndpoints (Proxy :: Proxy sub)
            currentSegment          = T.pack $ (':':) $ symbolVal (Proxy :: Proxy capture)
            qualify APIEndpoint{..} = APIEndpoint (currentSegment : pathSegments) method
        in
            map qualify endpoints

instance HasEndpoint (sub :: *) => HasEndpoint (Summary d :> sub) where
    getEndpoint        _ = getEndpoint        (Proxy :: Proxy sub)
    enumerateEndpoints _ = enumerateEndpoints (Proxy :: Proxy sub)

instance HasEndpoint (sub :: *) => HasEndpoint (Description d :> sub) where
    getEndpoint        _ = getEndpoint        (Proxy :: Proxy sub)
    enumerateEndpoints _ = enumerateEndpoints (Proxy :: Proxy sub)

instance HasEndpoint (sub :: *) => HasEndpoint (Header' mods h a :> sub) where
    getEndpoint        _ = getEndpoint        (Proxy :: Proxy sub)
    enumerateEndpoints _ = enumerateEndpoints (Proxy :: Proxy sub)

instance HasEndpoint (sub :: *) => HasEndpoint (QueryParam' mods (h :: Symbol) a :> sub) where
    getEndpoint        _ = getEndpoint        (Proxy :: Proxy sub)
    enumerateEndpoints _ = enumerateEndpoints (Proxy :: Proxy sub)

instance HasEndpoint (sub :: *) => HasEndpoint (QueryParams (h :: Symbol) a :> sub) where
    getEndpoint        _ = getEndpoint        (Proxy :: Proxy sub)
    enumerateEndpoints _ = enumerateEndpoints (Proxy :: Proxy sub)

instance HasEndpoint (sub :: *) => HasEndpoint (QueryFlag h :> sub) where
    getEndpoint        _ = getEndpoint        (Proxy :: Proxy sub)
    enumerateEndpoints _ = enumerateEndpoints (Proxy :: Proxy sub)

instance HasEndpoint (sub :: *) => HasEndpoint (ReqBody' mods cts a :> sub) where
    getEndpoint        _ = getEndpoint        (Proxy :: Proxy sub)
    enumerateEndpoints _ = enumerateEndpoints (Proxy :: Proxy sub)

#if MIN_VERSION_servant(0,15,0)
instance HasEndpoint (sub :: *) => HasEndpoint (StreamBody' mods framing ct a :> sub) where
    getEndpoint        _ = getEndpoint        (Proxy :: Proxy sub)
    enumerateEndpoints _ = enumerateEndpoints (Proxy :: Proxy sub)
#endif

instance HasEndpoint (sub :: *) => HasEndpoint (RemoteHost :> sub) where
    getEndpoint        _ = getEndpoint        (Proxy :: Proxy sub)
    enumerateEndpoints _ = enumerateEndpoints (Proxy :: Proxy sub)

instance HasEndpoint (sub :: *) => HasEndpoint (IsSecure :> sub) where
    getEndpoint        _ = getEndpoint        (Proxy :: Proxy sub)
    enumerateEndpoints _ = enumerateEndpoints (Proxy :: Proxy sub)

instance HasEndpoint (sub :: *) => HasEndpoint (HttpVersion :> sub) where
    getEndpoint        _ = getEndpoint        (Proxy :: Proxy sub)
    enumerateEndpoints _ = enumerateEndpoints (Proxy :: Proxy sub)

instance HasEndpoint (sub :: *) => HasEndpoint (Vault :> sub) where
    getEndpoint        _ = getEndpoint        (Proxy :: Proxy sub)
    enumerateEndpoints _ = enumerateEndpoints (Proxy :: Proxy sub)

instance HasEndpoint (sub :: *) => HasEndpoint (WithNamedContext x y sub) where
    getEndpoint        _ = getEndpoint        (Proxy :: Proxy sub)
    enumerateEndpoints _ = enumerateEndpoints (Proxy :: Proxy sub)

instance ReflectMethod method => HasEndpoint (Verb method status cts a) where
    getEndpoint _ req = case pathInfo req of
        [] | requestMethod req == method -> Just (APIEndpoint [] method)
        _                                -> Nothing
      where method = reflectMethod (Proxy :: Proxy method)

    enumerateEndpoints _ = [APIEndpoint mempty method]
      where method = reflectMethod (Proxy :: Proxy method)

instance ReflectMethod method => HasEndpoint (Stream method status framing ct a) where
    getEndpoint _ req = case pathInfo req of
        [] | requestMethod req == method -> Just (APIEndpoint [] method)
        _                                -> Nothing
      where method = reflectMethod (Proxy :: Proxy method)

    enumerateEndpoints _ = [APIEndpoint mempty method]
      where method = reflectMethod (Proxy :: Proxy method)

instance HasEndpoint Raw where
    getEndpoint      _ _ = Just (APIEndpoint [] "RAW")
    enumerateEndpoints _ =      [APIEndpoint [] "RAW"]

instance HasEndpoint (sub :: *) => HasEndpoint (CaptureAll (h :: Symbol) a :> sub) where
    getEndpoint        _ = getEndpoint        (Proxy :: Proxy sub)
    enumerateEndpoints _ = enumerateEndpoints (Proxy :: Proxy sub)
