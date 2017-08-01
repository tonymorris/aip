{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Aviation.Aip2 where

import Control.Applicative
import Control.Lens
import Data.Bool
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

----

requestAipTree ::
  ExceptT ConnError IO String
requestAipTree =
  let r = setRequestBody
            (aipRequestPost "aip.asp" "?pg=10")
            ("application/x-www-form-urlencoded", "Submit=I+Agree&check=1")
  in  ExceptT ((rspBody <$>) <$> simpleHTTP r)

aipTree ::
  String
  -> Aip0
aipTree =
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

aipBookTree ::
  String
  -> Maybe AipBookTypes
aipBookTree =
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

requestAipBooks ::
  Aip books charts supplementsaics summarysupaics daps dahs ersas precisionobstaclecharts
  -> Aip (Request String) charts supplementsaics summarysupaics daps dahs ersas precisionobstaclecharts
requestAipBooks (Aip (AipBooks books) charts supplementsaics summarysupaics daps dahs ersas precisionobstaclecharts) =
  let aipBookRequest ::
        AipBook a
        -> Request String
      aipBookRequest (AipBook _ _ (AipHref (AipPg p1 p2) (AipDate (Day dy1 dy2) m (Year y1 y2 y3 y4)) v) _) = 
        aipRequestGet
          "aip.asp"
          (
            concat
              [
                "?pg="
              , show p1
              , show p2
              , "&vdate="
              , show dy1
              , show dy2
              , "-"
              , show m
              , "-"
              , show y1
              , show y2
              , show y3
              , show y4
              , "&ver="
              , show v
              ]
          )
  in  Aip (AipBooks ((\b -> aipBookRequest b <$ b) <$> books)) charts supplementsaics summarysupaics daps dahs ersas precisionobstaclecharts

testRequestAipBooks =
  do  s <- requestAipTree
      let t = aipTree s
          u = requestAipBooks t
      undefined

testAipTree ::
  ExceptT ConnError IO Aip0
testAipTree =
  aipTree <$> requestAipTree

testAipBook ::
  ExceptT ConnError IO String
testAipBook =
  ExceptT ((rspBody <$>) <$> simpleHTTP (aipRequestGet "aip.asp" "?pg=20&vdate=25-May-2017&ver=1"))

----

data Link =
  Link
    String
  deriving (Eq, Ord, Show)

data Aip books charts supplementsaics summarysupaics daps dahs ersas precisionobstaclecharts =
  Aip {
    _books ::
      AipBooks books
  , _charts ::
      AipCharts charts
  , _supplementsaics ::
      AipSupplementsAICs supplementsaics
  , _summarysupaics ::
      AipSummarySUP_AICs summarysupaics      
  , _daps ::
      AipDAPs daps
  , _dahs ::
      AipDAHs dahs
  , _ersas ::
      AipERSAs ersas
  , _precisionobstaclecharts ::
      AipPrecisionObstacleCharts precisionobstaclecharts
  }
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

instance Functor AipBooks where
  fmap f (AipBooks x) =
    AipBooks ((f <$>) <$> x)
    
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

instance Functor AipBook where
  fmap f (AipBook s d h a) =
    AipBook s d h (f a)

data AipCharts a =
  AipCharts
    [AipChart a]
  deriving (Eq, Ord, Show)

instance Functor AipCharts where
  fmap f (AipCharts x) =
    AipCharts ((f <$>) <$> x)
    
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

instance Functor AipChart where
  fmap f (AipChart s d h a) =
    AipChart s d h (f a)

data AipSupplementsAICs a =
  AipSupplementsAICs
    [AipSupplementsAIC a]
  deriving (Eq, Ord, Show)

instance Functor AipSupplementsAICs where
  fmap f (AipSupplementsAICs x) =
    AipSupplementsAICs ((f <$>) <$> x)
    
data AipSupplementsAIC a =
  AipSupplementsAIC
    String
    AipPg
    a
  deriving (Eq, Ord, Show)

instance Functor AipSupplementsAIC where
  fmap f (AipSupplementsAIC s p a) =
    AipSupplementsAIC s p (f a)

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

instance Functor AipSummarySUP_AIC where
  fmap f (AipSummarySUP_AIC s1 s2 s3 a) =
    AipSummarySUP_AIC s1 s2 s3 (f a)

instance Monoid (AipSummarySUP_AICs a) where
  AipSummarySUP_AICs x `mappend` AipSummarySUP_AICs y =
    AipSummarySUP_AICs (x `mappend` y)
  mempty =
    AipSummarySUP_AICs mempty

data AipDAPs a =
  AipDAPs
    [AipDAP a]
  deriving (Eq, Ord, Show)

instance Functor AipDAPs where
  fmap f (AipDAPs x) =
    AipDAPs ((f <$>) <$> x)
    
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

instance Functor AipDAP where
  fmap f (AipDAP s d h a) =
    AipDAP s d h (f a)

data AipDAHs a =
  AipDAHs
    [AipDAH a]
  deriving (Eq, Ord, Show)

instance Functor AipDAHs where
  fmap f (AipDAHs x) =
    AipDAHs ((f <$>) <$> x)
    
data AipDAH a =
  AipDAH
    String
    String
    a
  deriving (Eq, Ord, Show)

instance Functor AipDAH where
  fmap f (AipDAH s1 s2 a) =
    AipDAH s1 s2 (f a)
    
instance Monoid (AipDAHs a) where
  AipDAHs x `mappend` AipDAHs y =
    AipDAHs (x `mappend` y)
  mempty =
    AipDAHs mempty

data AipERSAs a =
  AipERSAs
    [AipERSA a]
  deriving (Eq, Ord, Show)

instance Functor AipERSAs where
  fmap f (AipERSAs x) =
    AipERSAs ((f <$>) <$> x)
    
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

instance Functor AipERSA where
  fmap f (AipERSA s d h a) =
    AipERSA s d h (f a)

data AipPrecisionObstacleCharts a =
  AipPrecisionObstacleCharts
    [AipPrecisionObstacleChart a]
  deriving (Eq, Ord, Show)

instance Functor AipPrecisionObstacleCharts where
  fmap f (AipPrecisionObstacleCharts x) =
    AipPrecisionObstacleCharts ((f <$>) <$> x)
    
data AipPrecisionObstacleChart a =
  AipPrecisionObstacleChart
    String
    String
    a
  deriving (Eq, Ord, Show)

instance Functor AipPrecisionObstacleChart where
  fmap f (AipPrecisionObstacleChart s1 s2 a) =
    AipPrecisionObstacleChart s1 s2 (f a)
    
instance Monoid (AipPrecisionObstacleCharts a) where
  AipPrecisionObstacleCharts x `mappend` AipPrecisionObstacleCharts y =
    AipPrecisionObstacleCharts (x `mappend` y)
  mempty =
    AipPrecisionObstacleCharts mempty

data AipBookTypes =
  AipBookTypes {
    _bookcomplete ::
      String
  , _bookgeneral ::
      String
  , _bookenroute ::
      String
  , _bookaerodrome ::
      String
  , _bookindex ::
      String
  , _bookamendmentinstructions ::
      String
  } deriving (Eq, Ord, Show)

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

makeClassy ''Aip
makeWrapped ''AipBooks
makeClassy ''AipBook
makeWrapped ''AipCharts
makeClassy ''AipChart
makeWrapped ''AipSupplementsAICs
makeClassy ''AipSupplementsAIC
makeWrapped ''AipDAPs
makeClassy ''AipDAP
makeWrapped ''AipDAHs
makeClassy ''AipDAH
makeWrapped ''AipERSAs
makeClassy ''AipERSA
makeWrapped ''AipPrecisionObstacleCharts
makeClassy ''AipPrecisionObstacleChart
makeClassy ''AipBookTypes
makeClassy ''Day
makeClassy ''Month
makeClassy ''Year
