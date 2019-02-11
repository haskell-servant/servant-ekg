# servant-ekg

[![Build Status](https://travis-ci.org/haskell-servant/servant-ekg.png)](https://travis-ci.org/haskell-servant/servant-ekg)

# Servant Performance Counters

This package lets you track peformance counters for each of your Servant endpoints using EKG.

# Usage

Servant-EKG knows how to handle all official Servant combinators out of the box.

## Instrumenting your API
To use Servant-EKG, you'll need to wrap your WAI application with the Servant-EKG middleware.

```
import Network.Wai.Handler.Warp
import System.Metrics
import Servant.Ekg

wrapWithEkg :: Proxy api -> Server api -> IO Application
wrapWithEkg api server = do
  monitorEndpoints' <- monitorEndpoints api =<< newStore

  return $ monitorEndpoints' (serve api server)

main :: IO ()
main = do
  let api    = ...
      server = ...

  app <- wrapWithEkg api server

  run 8080 app
```

## Runtime overhead
Instrumenting your API introduces a non-zero runtime overhead, on the order of 200 - 600 µsec depending upon your machine. It's a good idea to run the benchmarks on your intended production platform to get an idea of how large the overhead will be. You'll need to have `wrk` installed to run the benchmarks.

In general, the runtime overhead should be effectively negligible if your handlers are issuing network requests, such as to databases. If you have handlers that are small, CPU-only, and requested frequently, you will see a performance hit from Servant-EKG.
