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

-- traverseTree :: Monoid m => (TagTreePos str -> m) -> TagTreePos str -> m 

traverseAip ::
  TagTreePos String
  -> [AipElement]
traverseAip t =
  let aipHref ('a':'i':'p':'.':'a':'s':'p':'?':'p':'g':'=':p1:p2:'&':'v':'d':'a':'t':'e':'=':d1:d2:'-':m1:m2:m3:'-':y1:y2:y3:y4:'&':v':'e':'r':'=':v:[]) =
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
  

-- [TagBranch "a" [("href","aip.asp?pg=20&vdate=25-May-2017&ver=1")] [TagLeaf (TagText "AIP Book")],TagLeaf (TagText " (25-May-2017)")]
