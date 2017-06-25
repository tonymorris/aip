module Data.Aviation.Aip where

import Data.Maybe
import Network.Stream
import Network.HTTP
import Network.URI
import Prelude
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree
import Text.HTML.TagSoup.Tree.Util
import Text.HTML.TagSoup.Tree.Zipper
import Control.Monad.Trans.Except


aipRequest ::
  String
  -> Request String
aipRequest s = 
  mkRequest GET (URI "http:" (Just (URIAuth "" "www.airservicesaustralia.com" "")) ("/aip/" ++ s) "" "")

aipRequestTree ::
  String
  -> ExceptT ConnError IO [TagTree String]
aipRequestTree s =
   ExceptT ((parseTree . rspBody <$>) <$> simpleHTTP (aipRequest s))

aipRequestTreePos ::
  String
  -> ExceptT ConnError IO (TagTreePos String)
aipRequestTreePos s =
  fromTagTree . htmlRoot <$> aipRequestTree s

---

request ::
  Request String
request = 
  let uri =
        URI "http:" (Just (URIAuth "" "www.airservicesaustralia.com" "")) "/aip/aip.asp" "?pg=10" ""
      headers =
        [
        ]
  in  setRequestBody (setHeaders (mkRequest POST uri) headers) ("application/x-www-form-urlencoded", "Submit=I+Agree&check=1")

aipTree ::
  ExceptT ConnError IO [TagTree String]
aipTree =
   ExceptT ((parseTree . rspBody <$>) <$> simpleHTTP request)

aipTreePos ::
  ExceptT ConnError IO (TagTreePos String)
aipTreePos =
  fromTagTree . htmlRoot <$> aipTree

data AipHref =
  AipHref
    Char
    Char
    Char
    Char
    Char
    Char
    Char
    Char
    Char
    Char
    Char
    Char
  deriving (Eq, Ord, Show)

data AipTag =
  AipTag
    String -- text
    AipHref -- href
  deriving (Eq, Ord, Show)

data AipType =
  Book
  | Charts
  | DAP
  | ERSA
  deriving (Eq, Ord, Show)

data AipElement =
  AipElement
    AipType
    AipTag
  deriving (Eq, Ord, Show)

traverseAip ::
  TagTreePos String
  -> [AipElement]
traverseAip t =
  let aipHref ('a':'i':'p':'.':'a':'s':'p':'?':'p':'g':'=':p1:p2:'&':'v':'d':'a':'t':'e':'=':d1:d2:'-':m1:m2:m3:'-':y1:y2:y3:y4:'&':'v':'e':'r':'=':v:[]) =
        Just (AipHref p1 p2 d1 d2 m1 m2 m3 y1 y2 y3 y4 v)
      aipHref _ =
        Nothing
      aipType "AIP Book" =
        Just Book
      aipType "AIP Charts" =
        Just Charts
      aipType "Departure and Approach Procedures (DAP)" =
        Just DAP
      aipType "En Route Supplement Australia (ERSA)" =
        Just ERSA
      aipType _ =
        Nothing
  in  case t of
        TagTreePos
          (TagBranch "a" [("href", href)] [TagLeaf (TagText n)])
          _
          [TagLeaf (TagText tx)]
          _ ->
            maybeToList $ 
              aipHref href >>= \h -> 
              aipType n    >>= \x ->
              return (AipElement x (AipTag tx h))
        _ ->
          []

aipHrefRequest ::
  AipHref
  -> Request String
aipHrefRequest (AipHref p1 p2 d1 d2 m1 m2 m3 y1 y2 y3 y4 v) = 
  let q =
        concat [
          "?pg="
        , [p1]
        , [p2]
        , "&vdate="
        , [d1]
        , [d2]
        , "-"
        , [m1]
        , [m2]
        , [m3]
        , "-"
        , [y1]
        , [y2]
        , [y3]
        , [y4]
        , "&ver="
        , [v]
        ]
      uri =
        URI "http:" (Just (URIAuth "" "www.airservicesaustralia.com" "")) "/aip/aip.asp" q ""
      headers =
        [
        ]
  in  setRequestBody (setHeaders (mkRequest POST uri) headers) ("application/x-www-form-urlencoded", "Submit=I+Agree&check=1")

aipHrefTree ::
  AipHref
  -> ExceptT ConnError IO [TagTree String]
aipHrefTree h =
   ExceptT ((parseTree . rspBody <$>) <$> simpleHTTP (aipHrefRequest h))

aipHrefTreePos ::
  AipHref
  -> ExceptT ConnError IO (TagTreePos String)
aipHrefTreePos h =
  fromTagTree . htmlRoot <$> aipHrefTree h

---- AIP Book

data AipBookElements =
  AipBookElements
    String -- href
    String -- name
  deriving (Eq, Ord, Show)

traverseAipBook ::
  TagTreePos String
  -> [AipBookElements]
traverseAipBook t =
  case t of
    TagTreePos
      (TagBranch "li" [] [TagBranch "a" [("href", h)] [TagLeaf (TagText x)]])
      _
      _
      _ ->
      [AipBookElements h x]
    TagTreePos
      (TagLeaf (TagComment "<li><a href=\"current/aip/index.pdf\">Index</a></li>"))
      _
      _
      _ ->
      [AipBookElements "current/aip/index.pdf" "Index"]
    _ ->
      []

---- AIP Charts

data AipChartElements =
  AipChartElements
    String -- href
    String -- name
  deriving (Eq, Ord, Show)

traverseAipCharts ::
  TagTreePos [Char]
  -> [AipChartElements]
traverseAipCharts t =
  case t of
    TagTreePos
      (TagBranch "li" [] [TagBranch "a" [("href", h)] [TagLeaf (TagText x)]])
      _
      _
      _ ->
      [AipChartElements h x]
    _ ->
      []

---- AIP Chart Sections

-- runExceptT $ traverseTree traverseAipCharts  <$> aipRequestTreePos "aip.asp?pg=60&vdate=25-May-2017&sect=ERCHigh&ver=1"

-- AIP ERSA

  
---- test values

-- aip.asp?pg=60&vdate=25-May-2017&sect=ERCHigh&ver=1

testBookHref :: 
  AipHref
testBookHref =
  AipHref '2' '0' '2' '5' 'M' 'a' 'y' '2' '0' '1' '7' '1'

testChartsHref ::
  AipHref
testChartsHref =
  AipHref '6' '0' '2' '5' 'M' 'a' 'y' '2' '0' '1' '7' '1'
