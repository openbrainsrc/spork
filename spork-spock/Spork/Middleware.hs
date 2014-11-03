{-# LANGUAGE OverloadedStrings #-}

module Spork.Middleware (
  corsMiddleware
  , setCorsHeaders
  , staticMiddleware
  , Network.Wai.Middleware.RequestLogger.logStdout
  ) where

import Data.Monoid
import Data.String

import Network.HTTP.Types.Method
import Network.HTTP.Types.Status
import Network.Wai
import Network.Wai.Middleware.Static
import qualified Network.Wai.Middleware.RequestLogger

import Web.Spock


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
    then k $ responseLBS status200 (mkCorsHeaders url) ""
    else app req k

setCorsHeaders :: URL -> SpockAction a b c ()
setCorsHeaders = mapM_ (uncurry setHeader) . mkCorsHeaders

mkCorsHeaders :: (IsString a, IsString b) => URL -> [(a, b)]
mkCorsHeaders url =
  let allowOrigin  = ( fromString "Access-Control-Allow-Origin"
                     , fromString $ url
                     )
      allowHeaders = ( fromString "Access-Control-Allow-Headers"
                     , fromString "Origin, X-Requested-With, Content-Type, Accept"
                     )
      allowMethods = ( fromString "Access-Control-Allow-Methods"
                     , fromString "GET, POST, PUT, OPTIONS, DELETE"
                     )
  in [allowOrigin, allowHeaders, allowMethods]
