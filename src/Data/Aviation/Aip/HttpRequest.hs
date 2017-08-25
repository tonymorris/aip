{-# LANGUAGE NoImplicitPrelude #-}

module Data.Aviation.Aip.HttpRequest(
  aipRequestGet
, aipRequestPost
, aipRequestMethod
, doRequest
, requestAipContents
) where
  
import Control.Monad.Trans.Except(ExceptT(ExceptT))
import Data.Aviation.Aip.ConnErrorHttp4xx(ConnErrorHttp4xx(IsConnError, Http4xx))
import Network.HTTP(HStream, Request, RequestMethod(GET, POST), mkRequest, setRequestBody, simpleHTTP, rspCode, rspBody)
import Network.BufferType(BufferType)
import Network.URI(URI(URI), URIAuth(URIAuth))
import Papa

aipRequestGet ::
  BufferType ty =>
  String
  -> String
  -> Request ty
aipRequestGet =
  aipRequestMethod GET

aipRequestPost ::
  BufferType ty =>
  String
  -> String
  -> Request ty
aipRequestPost =
  aipRequestMethod POST

aipRequestMethod ::
  BufferType ty =>
  RequestMethod
  -> String
  -> String
  -> Request ty
aipRequestMethod m s z =
  mkRequest m (URI "http:" (Just (URIAuth "" "www.airservicesaustralia.com" "")) ("/aip/" ++ s) z "")

doRequest ::
  HStream a =>
  Request a
  -> ExceptT ConnErrorHttp4xx IO a
doRequest r =
  ExceptT $
    do  x <- simpleHTTP r
        case x of
          Left e ->
            pure (Left (IsConnError e))
          Right c ->
            let (r1, r2, r3) = rspCode c
            in  if r1 == 4 then
                  pure (Left (Http4xx r2 r3))
                else
                  pure (Right (rspBody c))

requestAipContents ::
  ExceptT ConnErrorHttp4xx IO String
requestAipContents =
  let r = setRequestBody
            (aipRequestPost "aip.asp" "?pg=10")
            ("application/x-www-form-urlencoded", "Submit=I+Agree&check=1")
  in  doRequest r
