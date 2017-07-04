{-# LANGUAGE FlexibleContexts #-}

module Data.Aviation.Aip2 where

import Control.Applicative
import Control.Lens
import Data.Digit
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

aipRequest ::
  String
  -> Request String
aipRequest s = 
  mkRequest GET (URI "http:" (Just (URIAuth "" "www.airservicesaustralia.com" "")) ("/aip/" ++ s) "" "")

aipTree ::
  ExceptT ConnError IO [TagTree String]
aipTree =
  let r = setRequestBody
            (setHeaders (mkRequest POST (URI "http:" (Just (URIAuth "" "www.airservicesaustralia.com" "")) "/aip/aip.asp" "?pg=10" "")) [])
            ("application/x-www-form-urlencoded", "Submit=I+Agree&check=1")
  in  ExceptT ((parseTree . rspBody <$>) <$> simpleHTTP r)

aipTreePos ::
  ExceptT ConnError IO (TagTreePos String)
aipTreePos =
  fromTagTree . htmlRoot <$> aipTree

aipTreeTraversal ::
  TagTreePos String
  -> Aip () ()
aipTreeTraversal t =
  case t of
    TagTreePos
      (TagBranch "a" [("href", href)] [TagLeaf (TagText n)])
      _
      [TagLeaf (TagText tx)]
      _ ->
        case n of
          "AIP Book" ->
            case runParse parseAipHref href of
              Nothing ->
                mempty
              Just h ->
                Aip (AipBooks [AipBook h tx ()]) mempty
          "AIP Charts" ->
            case runParse parseAipHref href of
              Nothing ->
                mempty
              Just h ->
                Aip mempty (AipCharts [AipChart h tx ()])
          _ -> 
            mempty
    _ ->
      mempty -- []

getAipTree ::
  ExceptT ConnError IO (Aip () ())
getAipTree =
  traverseTree aipTreeTraversal <$> aipTreePos

----

data Link =
  Link
    String
  deriving (Eq, Ord, Show)

data Aip books charts =
  Aip
    (AipBooks books)
    (AipCharts charts)
    -- etc
  deriving (Eq, Ord, Show)

instance Monoid (Aip books charts) where
  Aip a1 a2 `mappend` Aip b1 b2 =
    Aip (a1 `mappend` b1) (a2 `mappend` b2)
  mempty =
    Aip mempty mempty
    
data AipBooks a =
  AipBooks
    [AipBook a]
  deriving (Eq, Ord, Show)

instance Monoid (AipBooks a) where
  AipBooks x `mappend` AipBooks y =
    AipBooks (x `mappend` y)
  mempty =
    AipBooks mempty

data AipBook a =
  AipBook
    AipHref
    String
    a
  deriving (Eq, Ord, Show)

data AipCharts a =
  AipCharts
    [AipChart a]
  deriving (Eq, Ord, Show)

instance Monoid (AipCharts a) where
  AipCharts x `mappend` AipCharts y =
    AipCharts (x `mappend` y)
  mempty =
    AipCharts mempty
    
data AipChart a =
  AipChart
    AipHref
    String
    a
  deriving (Eq, Ord, Show)

----

data Day =
  Day
    Digit
    Digit
  deriving (Eq, Ord, Show)

parseDay ::
  (CharParsing p, Monad p) =>
  p Day
parseDay =
  Day <$> parsedigit <*> parsedigit

data Month =
  Jan
  | Feb
  | Mar
  | Apr
  | May
  | Jun
  | Jul
  | Aug
  | Sep
  | Oct
  | Nov
  | Dec
  deriving (Eq, Ord, Show)

parseMonth ::
  CharParsing p =>
  p Month
parseMonth =
  choice
    [
      Jan <$ string "Jan"
    , Feb <$ try (string "Feb")
    , Mar <$ try (string "Mar")
    , Apr <$ try (string "Apr")
    , May <$ try (string "May")
    , Jun <$ try (string "Jun")
    , Jul <$ try (string "Jul")
    , Aug <$ try (string "Aug")
    , Sep <$ try (string "Sep")
    , Oct <$ try (string "Oct")
    , Nov <$ try (string "Nov")
    , Dec <$ try (string "Dec")
    ]

data Year =
  Year
    Digit
    Digit
    Digit
    Digit
  deriving (Eq, Ord, Show)

parseYear ::
  (CharParsing p, Monad p) =>
  p Year
parseYear =
  Year <$> parsedigit <*> parsedigit <*> parsedigit <*> parsedigit

data AipDate =
  AipDate
    Day
    Month
    Year
  deriving (Eq, Ord, Show)

parseAipDate ::
  (CharParsing p, Monad p) =>
  p AipDate
parseAipDate =
  AipDate <$> parseDay <* char '-' <*> parseMonth <* char '-' <*> parseYear

data AipPg =
  AipPg
    Digit
    Digit
  deriving (Eq, Ord, Show)

parseAipPg ::
  (CharParsing p, Monad p) =>
  p AipPg
parseAipPg =
  AipPg <$> parsedigit <*> parsedigit

data AipHref =
  AipHref
    AipPg
    AipDate
    Digit
  deriving (Eq, Ord, Show)
  
parseAipHref ::
  (CharParsing p, Monad p) =>
  p AipHref
parseAipHref =
  string "aip.asp?pg=" *> 
  (AipHref <$> parseAipPg <* string "&vdate=" <*> parseAipDate <* string "&ver=" <*> parsedigit)

runParse ::
  Stream s Identity t =>
  Parsec s () a
  -> s
  -> Maybe a
runParse p s =
  parse p "aip" s ^? _Right
