{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module Main (main) where

import           Data.Text                (Text)
import           Network.Wai              (Application)
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.Ekg
import           System.Metrics
import           System.Process


type BenchApi = "hello" :> Capture "name" Text :> Get '[JSON] Text

benchApi :: Proxy BenchApi
benchApi = Proxy

server :: Server BenchApi
server = return

servantEkgServer :: IO Application
servantEkgServer = do
  mware <- monitorEndpoints benchApi =<< newStore

  return $ mware (serve benchApi server)

benchApp :: IO Application -> IO ()
benchApp app = withApplication app $ \port ->
  callCommand $ "wrk -c 30 -d 20s --latency -s bench/wrk.lua -t 2 'http://localhost:" ++ show port ++ "'"

main :: IO ()
main = do
  putStrLn "Benchmarking servant-ekg"
  benchApp servantEkgServer
  putStrLn "Benchmarking without servant-ekg"
  benchApp . return $ serve benchApi server
