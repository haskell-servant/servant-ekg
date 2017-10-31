{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}

module Servant.EkgSpec (spec) where

import           Control.Concurrent
#if !MIN_VERSION_servant(0,9,0)
import           Control.Monad.Trans.Except
#endif
import           Data.Aeson
import           Data.Monoid
import           Data.Proxy
import qualified Data.HashMap.Strict as H
import           Data.Text
import           GHC.Generics
import           Network.HTTP.Client (defaultManagerSettings, newManager, Manager)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.Client
import           Servant.API.Internal.Test.ComprehensiveAPI (comprehensiveAPI)
import           System.Metrics
import qualified System.Metrics.Counter as Counter
import           Test.Hspec

import           Servant.Ekg


-- * Spec

spec :: Spec
spec = describe "servant-ekg" $ do

  let getEp :<|> postEp :<|> deleteEp = client testApi

  it "collects number of request" $ do
    withApp $ \port mvar -> do
      mgr <- newManager defaultManagerSettings
      let
#if MIN_VERSION_servant(0,9,0)
          runFn :: forall a. ClientM a -> IO (Either ServantError a)
          runFn fn = runClientM fn (ClientEnv mgr (BaseUrl Http "localhost" port ""))
#else
          runFn :: (Manager -> BaseUrl -> ExceptT e m a) -> m (Either e a)
          runFn fn = runExceptT $ fn mgr (BaseUrl Http "localhost" port "")
#endif
      _ <- runFn $ getEp "name" Nothing
      _ <- runFn $ postEp (Greet "hi")
      _ <- runFn $ deleteEp "blah"
      m <- readMVar mvar
      case H.lookup "hello.:name.GET" m of
        Nothing -> fail "Expected some value"
        Just v  -> Counter.read (metersC2XX v) `shouldReturn` 1
      case H.lookup "greet.POST" m of
        Nothing -> fail "Expected some value"
        Just v  -> Counter.read (metersC2XX v) `shouldReturn` 1
      case H.lookup "greet.:greetid.DELETE" m of
        Nothing -> fail "Expected some value"
        Just v  -> Counter.read (metersC2XX v) `shouldReturn` 1

  it "is comprehensive" $ do
    let _typeLevelTest = monitorEndpoints comprehensiveAPI undefined undefined undefined
    True `shouldBe` True


-- * Example

-- | A greet message data type
newtype Greet = Greet { _msg :: Text }
  deriving (Generic, Show)

instance FromJSON Greet
instance ToJSON Greet

-- API specification
type TestApi =
       -- GET /hello/:name?capital={true, false}  returns a Greet as JSON
       "hello" :> Capture "name" Text :> QueryParam "capital" Bool :> Get '[JSON] Greet

       -- POST /greet with a Greet as JSON in the request body,
       --             returns a Greet as JSON
  :<|> "greet" :> ReqBody '[JSON] Greet :> Post '[JSON] Greet

       -- DELETE /greet/:greetid
#if MIN_VERSION_servant(0,8,0)
  :<|> "greet" :> Capture "greetid" Text :> Delete '[JSON] NoContent
#else
  :<|> "greet" :> Capture "greetid" Text :> Delete '[JSON] ()
#endif

testApi :: Proxy TestApi
testApi = Proxy

-- Server-side handlers.
--
-- There's one handler per endpoint, which, just like in the type
-- that represents the API, are glued together using :<|>.
--
-- Each handler runs in the 'EitherT (Int, String) IO' monad.
server :: Server TestApi
server = helloH :<|> postGreetH :<|> deleteGreetH

  where helloH name Nothing = helloH name (Just False)
        helloH name (Just False) = return . Greet $ "Hello, " <> name
        helloH name (Just True) = return . Greet . toUpper $ "Hello, " <> name

        postGreetH = return

#if MIN_VERSION_servant(0,8,0)
        deleteGreetH _ = return NoContent
#else
        deleteGreetH _ = return ()
#endif

-- Turn the server into a WAI app. 'serve' is provided by servant,
-- more precisely by the Servant.Server module.
test :: Application
test = serve testApi server

withApp :: (Port -> MVar (H.HashMap Text Meters) -> IO a) -> IO a
withApp a = do
  ekg <- newStore
  ms <- newMVar mempty
  withApplication (return $ monitorEndpoints testApi ekg ms test) $ \p -> a p ms
