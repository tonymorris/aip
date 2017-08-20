{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Aviation.Aip where

import Control.Applicative
import Control.Lens
import Control.Monad.IO.Class
import Data.ByteString
import qualified Data.ByteString as ByteString
import Data.Maybe
import Network.BufferType
import Network.Stream hiding (Stream)
import Network.Download
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

data Ersa =
  Ersa
    AipHref
    AipDate
  deriving (Eq, Ord, Show)

newtype Ersas =
  Ersas
    [Ersa]
  deriving (Eq, Ord, Show)

instance Monoid Ersas where
  mempty =
    Ersas
      mempty
  Ersas x `mappend` Ersas y =
    Ersas (x `mappend` y)

parseAipTree ::
  String
  -> Ersas
parseAipTree =
  let aipTreeTraversal ::
        TagTreePos String
        -> Ersas
      aipTreeTraversal t =
        case t of
          TagTreePos (TagBranch "li" [] (TagBranch "a" [("href", href)] [TagLeaf (TagText n)]:TagLeaf (TagText tx):_)) _ _ _ ->
            let pdate = do  _ <- space
                            between (char '(') (char ')') parseAipDate
            in  case n of
                  "En Route Supplement Australia (ERSA)" ->
                    let p = do  h <- runParse parseAipHref href
                                d <- runParse pdate tx
                                pure (Ersas [Ersa h d])
                    in  fromMaybe mempty p
                  _ ->
                    mempty
          _ ->
            mempty
  in  traverseTree aipTreeTraversal . fromTagTree . htmlRoot . parseTree

runParse ::
  Stream s Identity t =>
  Parsec s () a
  -> s
  -> Maybe a
runParse p s =
  parse p "aip" s ^? _Right

{-
main = do
    jpg <- get "http://www.irregularwebcomic.net/comics/irreg2557.jpg"
    B.writeFile "irreg2557.jpg" jpg
  where
    get url = let uri = case parseURI url of
                          Nothing -> error $ "Invalid URI: " ++ url
                          Just u -> u in
              simpleHTTP (defaultGETRequest_ uri) >>= getResponseBody
-}

test2 ::
  ExceptT ConnError IO ()
test2 = 
  let g = aipRequestGet "current/aip/complete.pdf" ""
  in  doRequest g >>= liftIO . ByteString.writeFile "/tmp/h.pdf"

{-
# AIP Book (pending)

http://www.airservicesaustralia.com/aip/pending/aip/complete.pdf
http://www.airservicesaustralia.com/aip/pending/aip/general.pdf
http://www.airservicesaustralia.com/aip/pending/aip/enroute.pdf
http://www.airservicesaustralia.com/aip/pending/aip/aerodrome.pdf
http://www.airservicesaustralia.com/aip/pending/aip/cover.pdf

# AIP Book (current)

http://www.airservicesaustralia.com/aip/current/aip/complete.pdf
http://www.airservicesaustralia.com/aip/current/aip/general.pdf
http://www.airservicesaustralia.com/aip/current/aip/enroute.pdf
http://www.airservicesaustralia.com/aip/current/aip/aerodrome.pdf
http://www.airservicesaustralia.com/aip/current/aip/cover.pdf

# AIP Charts (current)

http://www.airservicesaustralia.com/aip/current/aipchart/erch/erch1.pdf
http://www.airservicesaustralia.com/aip/current/aipchart/erch/erch2.pdf
http://www.airservicesaustralia.com/aip/current/aipchart/erch/erch3.pdf
http://www.airservicesaustralia.com/aip/current/aipchart/erch/erch4.pdf
http://www.airservicesaustralia.com/aip/current/aipchart/erch/erch5.pdf
http://www.airservicesaustralia.com/aip/current/aipchart/ercl/ercl1.pdf
http://www.airservicesaustralia.com/aip/current/aipchart/ercl/ercl2.pdf
http://www.airservicesaustralia.com/aip/current/aipchart/ercl/ercl3.pdf
http://www.airservicesaustralia.com/aip/current/aipchart/ercl/ercl4.pdf
http://www.airservicesaustralia.com/aip/current/aipchart/ercl/ercl5.pdf
http://www.airservicesaustralia.com/aip/current/aipchart/ercl/ercl6.pdf
http://www.airservicesaustralia.com/aip/current/aipchart/ercl/ercl7.pdf
http://www.airservicesaustralia.com/aip/current/aipchart/ercl/ercl8.pdf
http://www.airservicesaustralia.com/aip/current/aipchart/pca/PCA_back.pdf
http://www.airservicesaustralia.com/aip/current/aipchart/pca/PCA_front.pdf
http://www.airservicesaustralia.com/aip/current/aipchart/tac/tac1.pdf
http://www.airservicesaustralia.com/aip/current/aipchart/tac/tac2.pdf
http://www.airservicesaustralia.com/aip/current/aipchart/tac/tac3.pdf
http://www.airservicesaustralia.com/aip/current/aipchart/tac/tac4.pdf
http://www.airservicesaustralia.com/aip/current/aipchart/tac/tac5.pdf
http://www.airservicesaustralia.com/aip/current/aipchart/tac/tac6.pdf
http://www.airservicesaustralia.com/aip/current/aipchart/tac/tac7.pdf
http://www.airservicesaustralia.com/aip/current/aipchart/tac/tac8.pdf
http://www.airservicesaustralia.com/aip/current/aipchart/vnc/Adelaide_VNC.pdf
http://www.airservicesaustralia.com/aip/current/aipchart/vnc/Brisbane_VNC.pdf
http://www.airservicesaustralia.com/aip/current/aipchart/vnc/Bundaberg_VNC.pdf
http://www.airservicesaustralia.com/aip/current/aipchart/vnc/Cairns_VNC.pdf
http://www.airservicesaustralia.com/aip/current/aipchart/vnc/Darwin_VNC.pdf
http://www.airservicesaustralia.com/aip/current/aipchart/vnc/Deniliquin_VNC.pdf
http://www.airservicesaustralia.com/aip/current/aipchart/vnc/Hobart_VNC.pdf
http://www.airservicesaustralia.com/aip/current/aipchart/vnc/Launceston_VNC.pdf
http://www.airservicesaustralia.com/aip/current/aipchart/vnc/Melbourne_VNC.pdf
http://www.airservicesaustralia.com/aip/current/aipchart/vnc/Newcastle_VNC.pdf
http://www.airservicesaustralia.com/aip/current/aipchart/vnc/Perth_VNC.pdf
http://www.airservicesaustralia.com/aip/current/aipchart/vnc/Rockhampton_VNC.pdf
http://www.airservicesaustralia.com/aip/current/aipchart/vnc/Sydney_VNC.pdf
http://www.airservicesaustralia.com/aip/current/aipchart/vnc/Tindal_VNC.pdf
http://www.airservicesaustralia.com/aip/current/aipchart/vnc/Townsville_VNC.pdf
http://www.airservicesaustralia.com/aip/current/aipchart/vtc/Adelaide_VTC.pdf
http://www.airservicesaustralia.com/aip/current/aipchart/vtc/Albury_VTC.pdf
http://www.airservicesaustralia.com/aip/current/aipchart/vtc/AliceSprings_Uluru_VTC.pdf
http://www.airservicesaustralia.com/aip/current/aipchart/vtc/Brisbane_Sunshine_VTC.pdf
http://www.airservicesaustralia.com/aip/current/aipchart/vtc/Broome_VTC.pdf
http://www.airservicesaustralia.com/aip/current/aipchart/vtc/Cairns_VTC.pdf
http://www.airservicesaustralia.com/aip/current/aipchart/vtc/Canberra_VTC.pdf
http://www.airservicesaustralia.com/aip/current/aipchart/vtc/Coffs_Harbour_VTC.pdf
http://www.airservicesaustralia.com/aip/current/aipchart/vtc/Darwin_VTC.pdf
http://www.airservicesaustralia.com/aip/current/aipchart/vtc/Gold_Coast_VTC.pdf
http://www.airservicesaustralia.com/aip/current/aipchart/vtc/Hobart_VTC.pdf
http://www.airservicesaustralia.com/aip/current/aipchart/vtc/Karratha_VTC.pdf
http://www.airservicesaustralia.com/aip/current/aipchart/vtc/Launceston_VTC.pdf
http://www.airservicesaustralia.com/aip/current/aipchart/vtc/Mackay_VTC.pdf
http://www.airservicesaustralia.com/aip/current/aipchart/vtc/Melbourne_VTC.pdf
http://www.airservicesaustralia.com/aip/current/aipchart/vtc/Newcastle_Williamtown_VTC.pdf
http://www.airservicesaustralia.com/aip/current/aipchart/vtc/Oakey_Bris_VTC.pdf
http://www.airservicesaustralia.com/aip/current/aipchart/vtc/perth_legend.pdf
http://www.airservicesaustralia.com/aip/current/aipchart/vtc/Perth_VTC.pdf
http://www.airservicesaustralia.com/aip/current/aipchart/vtc/Rockhampton_VTC.pdf
http://www.airservicesaustralia.com/aip/current/aipchart/vtc/Sydney_VTC.pdf
http://www.airservicesaustralia.com/aip/current/aipchart/vtc/Tamworth_VTC.pdf
http://www.airservicesaustralia.com/aip/current/aipchart/vtc/Townsville_VTC.pdf
http://www.airservicesaustralia.com/aip/current/aipchart/vtc/Whitsunday_VTC.pdf

# DAP (current)

http://www.airservicesaustralia.com/aip/current/dap/SpecNotManTOC.htm
http://www.airservicesaustralia.com/aip/current/dap/ChecklistTOC.htm
http://www.airservicesaustralia.com/aip/current/dap/LegendInfoTablesTOC.htm
http://www.airservicesaustralia.com/aip/current/dap/AeroProcChartsTOC.htm

# DAP (pending)

http://www.airservicesaustralia.com/aip/pending/dap/SpecNotManTOC.htm
http://www.airservicesaustralia.com/aip/pending/dap/ChecklistTOC.htm
http://www.airservicesaustralia.com/aip/pending/dap/LegendInfoTablesTOC.htm
http://www.airservicesaustralia.com/aip/pending/dap/AeroProcChartsTOC.htm

# DAH (current)

http://www.airservicesaustralia.com/aip/current/dah/dah.pdf

# ERSA

\(dd-mmm-yyyy) ->
  …

# Precision Approach Terrain Charts and Type A & Type B Obstacle Charts

http://www.airservicesaustralia.com/aip/current/chart/TypeAandBCharts.pdf
-}
