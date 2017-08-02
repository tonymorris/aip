module Data.Aviation.Aip.Types.Aip where

import Control.Applicative
import Control.Lens
import Data.Aviation.Aip.Types.AipBook
import Data.Aviation.Aip.Types.AipBooks
import Data.Aviation.Aip.Types.AipBookTypes
import Data.Aviation.Aip.Types.AipChart
import Data.Aviation.Aip.Types.AipCharts
import Data.Aviation.Aip.Types.AipDAH
import Data.Aviation.Aip.Types.AipDAHs
import Data.Aviation.Aip.Types.AipDAP
import Data.Aviation.Aip.Types.AipDAPs
import Data.Aviation.Aip.Types.AipERSA
import Data.Aviation.Aip.Types.AipERSAs
import Data.Aviation.Aip.Types.AipPrecisionObstacleChart
import Data.Aviation.Aip.Types.AipPrecisionObstacleCharts
import Data.Aviation.Aip.Types.AipSummarySUP_AIC
import Data.Aviation.Aip.Types.AipSummarySUP_AICs
import Data.Aviation.Aip.Types.AipSupplementsAIC
import Data.Aviation.Aip.Types.AipSupplementsAICs
import Data.Digit
import Prelude
import Text.Parser.Char
import Text.Parser.Combinators

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
