{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Yesod.Katip.Orphans () where

import Data.Aeson
import Katip (LogItem (..), PayloadSelection (..), ToObject (..), Verbosity (..))
import Network.Wai

#if MIN_VERSION_wai_extra(3, 1, 4)
import Network.Wai.Middleware.RequestLogger.JSON (requestToJSON)
#else
-- Because the exposure of this particular API in Wai is relatively recent
-- (December 2020), this polyfills the implementation
import Data.CaseInsensitive (original)
import Data.IP (fromHostAddress, fromIPv4)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Network.HTTP.Types
import Network.Socket (SockAddr (..))

requestToJSON :: Request -> [String] -> Maybe () -> Value
requestToJSON req body _duration
  = object
      [ "method" .= decodeUtf8With lenientDecode (requestMethod req)
      , "path" .= decodeUtf8With lenientDecode (rawPathInfo req)
      , "queryString" .= map queryItemToJSON (queryString req)
      , "size" .= requestBodyLengthToJSON (requestBodyLength req)
      , "body" .= concat body
      , "remoteHost" .= sockToJSON (remoteHost req)
      , "httpVersion" .= httpVersionToJSON (httpVersion req)
      , "headers" .= requestHeadersToJSON (requestHeaders req)
      ]
  where
    requestHeadersToJSON = toJSON . map hToJ where
      -- Redact cookies
      hToJ ("Cookie", _) = toJSON ("Cookie" :: T.Text, "-RDCT-" :: T.Text)
      hToJ hd = headerToJSON hd

    queryItemToJSON (name, mValue) = toJSON (decodeUtf8With lenientDecode name, fmap (decodeUtf8With lenientDecode) mValue)

    requestBodyLengthToJSON ChunkedBody = String "Unknown"
    requestBodyLengthToJSON (KnownLength l) = toJSON l

    sockToJSON (SockAddrInet pn ha) =
      object
        [ "port" .= portToJSON pn
        , "hostAddress" .= word32ToHostAddress ha
        ]
    sockToJSON (SockAddrInet6 pn _ ha _) =
      object
        [ "port" .= portToJSON pn
        , "hostAddress" .= ha
        ]
    sockToJSON (SockAddrUnix sock) =
      object [ "unix" .= sock ]
#if !MIN_VERSION_network(3, 0, 0)
    sockToJSON (SockAddrCan i) =
      object [ "can" .= i ]
#endif

    headerToJSON (headerName, header) = toJSON (decodeUtf8With lenientDecode . original $ headerName, decodeUtf8With lenientDecode header)

    word32ToHostAddress = T.intercalate "." . map (T.pack . show) . fromIPv4 . fromHostAddress

    portToJSON = toJSON . toInteger

    httpVersionToJSON (HttpVersion major minor) = String $ T.pack (show major) <> "." <> T.pack (show minor)
#endif

instance ToObject Request where
  toObject req
    = case requestToJSON req ["<omitted by default>"] Nothing of
        Object obj -> obj
        _ -> error "`requestToJSON` produced a JSON representation for `Request` that wasn't an object!"

instance LogItem Request where
  payloadKeys verbosity _req = case verbosity of
    V0 -> SomeKeys ["method", "path", "queryString", "remotehost"]
    V1 -> SomeKeys ["size", "body"]
    V2 -> SomeKeys ["headers", "httpVersion"]
    V3 -> AllKeys
