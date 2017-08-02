{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Aviation.Aip where

import Control.Applicative
import Control.Lens
import Data.Bool
import Data.Maybe
import Network.Stream hiding (Stream)
import Network.HTTP
import Network.URI
import Prelude
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree
import Text.HTML.TagSoup.Tree.Util
import Text.HTML.TagSoup.Tree.Zipper
import Text.Parsec(Parsec, Stream, parse)
import Text.Parser.Char
import Text.Parser.Combinators
import Control.Monad.Trans.Except
import Data.Aviation.Aip.Types

----

aipRequest ::
  String
  -> Request String
aipRequest s =
  aipRequestGet s ""

aipRequestGet ::
  String
  -> String
  -> Request String
aipRequestGet =
  aipRequestMethod GET

aipRequestPost ::
  String
  -> String
  -> Request String
aipRequestPost =
  aipRequestMethod POST

aipRequestMethod ::
  RequestMethod
  -> String
  -> String
  -> Request String
aipRequestMethod m s z =
  mkRequest m (URI "http:" (Just (URIAuth "" "www.airservicesaustralia.com" "")) ("/aip/" ++ s) z "")

doRequest ::
  HStream a =>
  Request a
  -> ExceptT ConnError IO a
doRequest r =
  ExceptT ((rspBody <$>) <$> simpleHTTP r)

----

requestAipContents ::
  ExceptT ConnError IO String
requestAipContents =
  let r = setRequestBody
            (aipRequestPost "aip.asp" "?pg=10")
            ("application/x-www-form-urlencoded", "Submit=I+Agree&check=1")
  in  doRequest r

parseAipTree ::
  String
  -> Aip0
parseAipTree =
  let aipTreeTraversal ::
        TagTreePos String
        -> Aip0
      aipTreeTraversal t =
        case t of
          TagTreePos (TagBranch "li" [] [TagBranch "a" [("href", href)] [TagLeaf (TagText n)]]) _ _ _ ->
            case n of
              "AIP Supplements and AICs" ->
                let p = do  g <- runParse parseAipPgHref href
                            pure (oneAipSupplementsAIC (AipSupplementsAIC n g ()))
                in  fromMaybe mempty p
              'S':'u':'m':'m':'a':'r':'y':' ':'o':'f':' ':'S':'U':'P':'/':'A':'I':'C':' ':'C':'u':'r':'r':'e':'n':'t':' ':r ->
                oneAipSummarySUP_AIC (AipSummarySUP_AIC n href r ())
              "Precision Approach Terrain Charts and Type A & Type B Obstacle Charts" ->
                oneAipPrecisionObstacleChart (AipPrecisionObstacleChart n href ())
              _ ->
                mempty
          TagTreePos (TagBranch "li" [] (TagBranch "a" [("href", href)] [TagLeaf (TagText n)]:TagLeaf (TagText tx):_)) _ _ _ ->
            let pdate = do  _ <- space
                            between (char '(') (char ')') parseAipDate
            in  case n of
                  "AIP Book" ->
                    let p = do  h <- runParse parseAipHref href
                                d <- runParse pdate tx
                                pure (oneAipBook (AipBook n d h ()))
                    in  fromMaybe mempty p   
                  "AIP Charts" ->
                    let p = do  h <- runParse parseAipHref href
                                d <- runParse pdate tx
                                pure (oneAipChart (AipChart n d h ()))
                    in  fromMaybe mempty p
                  "Departure and Approach Procedures (DAP)" ->
                    let p = do  h <- runParse parseAipHref href
                                d <- runParse pdate tx
                                pure (oneAipDAP (AipDAP n d h ()))
                    in  fromMaybe mempty p
                  "Designated Airspace Handbook (DAH)" ->
                    oneAipDAH (AipDAH n href ())
                  "En Route Supplement Australia (ERSA)" ->
                    let p = do  h <- runParse parseAipHref href
                                d <- runParse pdate tx
                                pure (oneAipERSA (AipERSA n d h ()))
                    in  fromMaybe mempty p   
                  _ ->
                    mempty
          _ ->
            mempty

  in  traverseTree aipTreeTraversal . fromTagTree . htmlRoot . parseTree

parseBookTree ::
  String
  -> Maybe AipBookTypes
parseBookTree =
  let aipBookTreeTraversed ::
        TagTreePos String
        -> Maybe AipBookTypes
      aipBookTreeTraversed t =
        let trav t' =   case t' of
                          TagTreePos
                            (
                              TagBranch "ul" []
                              [
                                TagLeaf (TagText _)
                              , TagBranch "li" [] [TagBranch "a" [("href", completeHref)] [TagLeaf (TagText "Complete")]]
                              , TagLeaf (TagText _)
                              , TagBranch "li" [] [TagBranch "a" [("href", generalHref)] [TagLeaf (TagText "General")]]
                              , TagLeaf (TagText _)
                              , TagBranch "li" [] [TagBranch "a" [("href", enrouteHref)] [TagLeaf (TagText "En Route")]]
                              , TagLeaf (TagText _)
                              , TagBranch "li" [] [TagBranch "a" [("href", aerodromeHref)] [TagLeaf (TagText "Aerodrome")]]
                              , TagLeaf (TagText _)
                              , TagLeaf (TagComment ind)
                              , TagLeaf (TagText _)
                              , TagBranch "li" [] [TagBranch "a" [("href", coverHref)] [TagLeaf (TagText "Amendment Instructions")]]
                              , TagLeaf (TagText _)
                              ]
                            )
                            _ _ _ ->
                              let (i, j) =
                                    break (== '"') ind
                                  ind' =
                                    bool mempty (takeWhile (/= '"') . drop 1 $ j) (i == "<li><a href=")
                              in  (([completeHref], [generalHref], [enrouteHref]), ([aerodromeHref], [ind'], [coverHref]))
                          _ ->
                            mempty
        in  case traverseTree trav t of
              (([c], [g], [e]), ([a], [i], [m])) ->
                Just
                  (AipBookTypes c g e a i m)
              _ ->
                Nothing
  in  aipBookTreeTraversed . fromTagTree . htmlRoot . parseTree

requestAipBookTree ::
  Applicative f =>
  (Request String -> f String)
  -> Aip books charts supplementsaics summarysupaics daps dahs ersas precisionobstaclecharts
  -> f (Aip (Maybe AipBookTypes) charts supplementsaics summarysupaics daps dahs ersas precisionobstaclecharts)
requestAipBookTree f =
  let aipBookRequest ::
        AipBook a
        -> Request String
      aipBookRequest = 
        aipRequestGet "aip.asp" . uriAipHref
  in  books (_Wrapped (traverse (traverse ((parseBookTree <$>) . f)))) . over (books . _Wrapped) ((\b -> aipBookRequest b <$ b) <$>)

testRequestAipBooks ::
  ExceptT ConnError IO (Aip (Maybe AipBookTypes) () () () () () () ())
testRequestAipBooks =
  requestAipContents >>= requestAipBookTree doRequest . parseAipTree

runParse ::
  Stream s Identity t =>
  Parsec s () a
  -> s
  -> Maybe a
runParse p s =
  parse p "aip" s ^? _Right
