{-# LANGUAGE OverloadedStrings #-}

module Spork.Middleware (
  corsMiddleware
  , setCorsHeaders
  , staticMiddleware
  , syslogLogger
  , Network.Wai.Middleware.RequestLogger.logStdout
  ) where

import Data.Monoid
import Data.String

import Network.HTTP.Types.Method
import Network.HTTP.Types.Status
import Network.Wai
import Network.Wai.Middleware.Static
import Network.Wai.Middleware.RequestLogger

import Web.Spock.Simple
import System.Posix.Syslog
import Data.Default
import Control.Monad.Trans

import qualified Data.ByteString.Char8 as BS8
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text as T
import System.Log.FastLogger


syslogLogger :: MonadIO m => m Middleware
syslogLogger = liftIO $ mkRequestLogger def { destination = Callback syslogit }
  where syslogit logstr = syslog System.Posix.Syslog.Info $ T.unpack $ decodeUtf8 $ fromLogStr logstr


staticMiddleware :: Maybe Policy -> [FilePath] -> Middleware
staticMiddleware mPolicy dirs =
    let middlewares = concatMap mkMiddlewares dirs
    in foldr (.) id middlewares
  where
    mkMiddlewares :: FilePath -> [Middleware]
    mkMiddlewares dir =
      let policy = maybe mempty id mPolicy
          --indexMiddleware     = staticPolicy $ only [("", dir </> "index.html")]
          otherFileMiddleware = staticPolicy $ policy >-> noDots >-> isNotAbsolute >-> addBase dir
      in [otherFileMiddleware]

type URL = String

corsMiddleware :: URL -> Application -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
corsMiddleware url app req k =
  if requestMethod req == methodOptions
    then k $ responseLBS status200 (mkCorsHeaders req) ""
    else app req k

setCorsHeaders :: Request -> SpockAction a b c ()
setCorsHeaders = mapM_ (uncurry setHeader) . mkCorsHeaders

type Origin = BS8.ByteString

-- TODO pass this from config and check against it
allowedOrigins :: [Origin]
allowedOrigins = [ "http://???" ]

-- https://www.w3.org/TR/cors/#access-control-allow-origin-response-header
allOrigins :: Origin
allOrigins = "*" -- does not works with cors requests with credentials

nullOrigin :: Origin
nullOrigin = "null"

refToOrigin ::  Maybe BS8.ByteString -> Origin
refToOrigin Nothing = nullOrigin
refToOrigin (Just ref) = origin
  where
    chunks = BS8.split '/' ref -- http://tools.ietf.org/html/rfc6454#section-7.1
    origin = if length chunks >= 3 then BS8.intercalate "/" (take 3 chunks)
                                   else nullOrigin

mkCorsHeaders :: (IsString a, IsString b) => Request -> [(a, b)]
mkCorsHeaders req =
  let allowOrigin  = ( fromString "Access-Control-Allow-Origin"
                     , fromString . BS8.unpack . refToOrigin . requestHeaderReferer $ req
                     )
      allowHeaders = ( fromString "Access-Control-Allow-Headers"
                     , fromString "Origin, X-Requested-With, Content-Type, Accept, Authorization"
                     )
      allowMethods = ( fromString "Access-Control-Allow-Methods"
                     , fromString "GET, POST, PUT, OPTIONS, DELETE"
                     )
      allowCredentials = ( fromString "Access-Control-Allow-Credentials"
                         , fromString "true"
                         )


  in [allowOrigin, allowHeaders, allowMethods, allowCredentials]
