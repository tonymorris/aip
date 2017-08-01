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

getAipTree ::
  ExceptT ConnError IO String
getAipTree =
  let r = setRequestBody
            (aipRequestPost "aip.asp" "?pg=10")
            ("application/x-www-form-urlencoded", "Submit=I+Agree&check=1")
  in  ExceptT ((rspBody <$>) <$> simpleHTTP r)

aipTreeTraversal ::
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

aipTree ::
  String
  -> Aip0
aipTree =
  traverseTree aipTreeTraversal . fromTagTree . htmlRoot . parseTree

testAipTree ::
  ExceptT ConnError IO Aip0
testAipTree =
  aipTree <$> getAipTree

----

data Link =
  Link
    String
  deriving (Eq, Ord, Show)

data Aip books charts supplementsaics summarysupaics daps dahs ersas precisionobstaclecharts =
  Aip
    (AipBooks books)
    (AipCharts charts)
    (AipSupplementsAICs supplementsaics)
    (AipSummarySUP_AICs summarysupaics)
    (AipDAPs daps)
    (AipDAHs dahs)
    (AipERSAs ersas)
    (AipPrecisionObstacleCharts precisionobstaclecharts)
  deriving (Eq, Ord, Show)

type Aip0 =
  Aip () () () () () () () ()

instance Monoid (Aip books charts supplementsaics summarysupaics daps dahs ersas precisionobstaclecharts) where
  Aip a1 a2 a3 a4 a5 a6 a7 a8 `mappend` Aip b1 b2 b3 b4 b5 b6 b7 b8 =
    Aip
      (a1 `mappend` b1)
      (a2 `mappend` b2)
      (a3 `mappend` b3)
      (a4 `mappend` b4)
      (a5 `mappend` b5)
      (a6 `mappend` b6)
      (a7 `mappend` b7)
      (a8 `mappend` b8)
  mempty =
    Aip
      mempty
      mempty
      mempty
      mempty
      mempty
      mempty
      mempty
      mempty

oneAipBook ::
  AipBook books
  -> Aip books charts supplementsaics summarysupaics daps dahs ersas precisionobstaclecharts
oneAipBook book =
  Aip (AipBooks [book]) mempty mempty mempty mempty mempty mempty mempty

oneAipChart ::
  AipChart charts
  -> Aip books charts supplementsaics summarysupaics daps dahs ersas precisionobstaclecharts
oneAipChart chart =
  Aip mempty (AipCharts [chart]) mempty mempty mempty mempty mempty mempty

oneAipSupplementsAIC ::
  AipSupplementsAIC supplementsaics
  -> Aip books charts supplementsaics summarysupaics daps dahs ersas precisionobstaclecharts
oneAipSupplementsAIC supplementsaic =
  Aip mempty mempty (AipSupplementsAICs [supplementsaic]) mempty mempty mempty mempty mempty

oneAipSummarySUP_AIC ::
  AipSummarySUP_AIC summarysupaics
  -> Aip books charts supplementsaics summarysupaics daps dahs ersas precisionobstaclecharts
oneAipSummarySUP_AIC summarysupaic =
  Aip mempty mempty mempty (AipSummarySUP_AICs [summarysupaic]) mempty mempty mempty mempty

oneAipDAP ::
  AipDAP daps
  -> Aip books charts supplementsaics summarysupaics daps dahs ersas precisionobstaclecharts
oneAipDAP dap =
  Aip mempty mempty mempty mempty (AipDAPs [dap]) mempty mempty mempty

oneAipDAH ::
  AipDAH dahs
  -> Aip books charts supplementsaics summarysupaics daps dahs ersas precisionobstaclecharts
oneAipDAH dah =
  Aip mempty mempty mempty mempty mempty (AipDAHs [dah]) mempty mempty

oneAipERSA ::
  AipERSA ersas
  -> Aip books charts supplementsaics summarysupaics daps dahs ersas precisionobstaclecharts
oneAipERSA ersa =
  Aip mempty mempty mempty mempty mempty mempty (AipERSAs [ersa]) mempty

oneAipPrecisionObstacleChart ::
  AipPrecisionObstacleChart precisionobstaclecharts
  -> Aip books charts supplementsaics summarysupaics daps dahs ersas precisionobstaclecharts
oneAipPrecisionObstacleChart precisionobstaclechart =
  Aip mempty mempty mempty mempty mempty mempty mempty (AipPrecisionObstacleCharts [precisionobstaclechart])

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
    String
    AipDate
    AipHref
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
    String
    AipDate
    AipHref
    a
  deriving (Eq, Ord, Show)

data AipSupplementsAICs a =
  AipSupplementsAICs
    [AipSupplementsAIC a]
  deriving (Eq, Ord, Show)

data AipSupplementsAIC a =
  AipSupplementsAIC
    String
    AipPg
    a
  deriving (Eq, Ord, Show)

instance Monoid (AipSupplementsAICs a) where
  AipSupplementsAICs x `mappend` AipSupplementsAICs y =
    AipSupplementsAICs (x `mappend` y)
  mempty =
    AipSupplementsAICs mempty

data AipSummarySUP_AICs a =
  AipSummarySUP_AICs
    [AipSummarySUP_AIC a]
  deriving (Eq, Ord, Show)

data AipSummarySUP_AIC a =
  AipSummarySUP_AIC
    String
    String
    String
    a
  deriving (Eq, Ord, Show)

instance Monoid (AipSummarySUP_AICs a) where
  AipSummarySUP_AICs x `mappend` AipSummarySUP_AICs y =
    AipSummarySUP_AICs (x `mappend` y)
  mempty =
    AipSummarySUP_AICs mempty

data AipDAPs a =
  AipDAPs
    [AipDAP a]
  deriving (Eq, Ord, Show)

instance Monoid (AipDAPs a) where
  AipDAPs x `mappend` AipDAPs y =
    AipDAPs (x `mappend` y)
  mempty =
    AipDAPs mempty

data AipDAP a =
  AipDAP
    String
    AipDate
    AipHref
    a
  deriving (Eq, Ord, Show)

data AipDAHs a =
  AipDAHs
    [AipDAH a]
  deriving (Eq, Ord, Show)

data AipDAH a =
  AipDAH
    String
    String
    a
  deriving (Eq, Ord, Show)

instance Monoid (AipDAHs a) where
  AipDAHs x `mappend` AipDAHs y =
    AipDAHs (x `mappend` y)
  mempty =
    AipDAHs mempty

data AipERSAs a =
  AipERSAs
    [AipERSA a]
  deriving (Eq, Ord, Show)

instance Monoid (AipERSAs a) where
  AipERSAs x `mappend` AipERSAs y =
    AipERSAs (x `mappend` y)
  mempty =
    AipERSAs mempty

data AipERSA a =
  AipERSA
    String
    AipDate
    AipHref
    a
  deriving (Eq, Ord, Show)

data AipPrecisionObstacleCharts a =
  AipPrecisionObstacleCharts
    [AipPrecisionObstacleChart a]
  deriving (Eq, Ord, Show)

data AipPrecisionObstacleChart a =
  AipPrecisionObstacleChart
    String
    String
    a
  deriving (Eq, Ord, Show)

instance Monoid (AipPrecisionObstacleCharts a) where
  AipPrecisionObstacleCharts x `mappend` AipPrecisionObstacleCharts y =
    AipPrecisionObstacleCharts (x `mappend` y)
  mempty =
    AipPrecisionObstacleCharts mempty

data AipBookType =
  Complete
  | General
  | EnRoute
  | Aerodrome
  | AmendmentInstructions
  deriving (Eq, Ord, Show)

data AipBookTypeHref =
  AipBookTypeHref
    AipBookType
    String
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

parseAipPgHref ::
  (CharParsing p, Monad p) =>
  p AipPg
parseAipPgHref =
  string "aip.asp?pg=" *> 
  parseAipPg

runParse ::
  Stream s Identity t =>
  Parsec s () a
  -> s
  -> Maybe a
runParse p s =
  parse p "aip" s ^? _Right
