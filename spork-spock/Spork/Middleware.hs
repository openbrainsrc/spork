{-# LANGUAGE OverloadedStrings #-}

module Spork.Middleware (
  corsMiddleware
  , setCorsHeaders
  , staticMiddleware
  , syslogLogger
  , Network.Wai.Middleware.RequestLogger.logStdout
  ) where

import Data.List (elemIndex)
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

corsMiddleware :: Middleware
corsMiddleware app req respond = app req $ respond . mapResponseHeaders (++(mkCorsHeaders req))

setCorsHeaders :: SpockAction a b c ()
setCorsHeaders = request >>= (mapM_ (uncurry setHeader) . mkCorsHeaders)

type Origin = BS8.ByteString

-- TODO pass this from config and check against it
allowedOrigins :: [Origin]
allowedOrigins = [ "http://???" ]

-- https://www.w3.org/TR/cors/#access-control-allow-origin-response-header
allOrigins :: Origin
allOrigins = "*" -- does not works with cors requests with credentials

nullOrigin :: Origin
nullOrigin = "null"

reqToOrigin :: Request -> Origin
reqToOrigin req = origin
  where
    hdrOrigin = lookup "origin" (requestHeaders req)
    origin = case hdrOrigin of
      Nothing -> nullOrigin
      Just x  -> x

checkOrigin :: Origin -> Origin
checkOrigin o = case elemIndex o allowedOrigins of
  Just _  -> o
  Nothing -> o -- TODO nullOrigin

mkCorsHeaders :: (IsString a, IsString b) => Request -> [(a, b)]
mkCorsHeaders req =
  let allowOrigin  = ( fromString "Access-Control-Allow-Origin"
                     , fromString . BS8.unpack . checkOrigin . reqToOrigin $ req
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
