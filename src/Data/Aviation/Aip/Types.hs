{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Aviation.Aip.Types(
    module A
  ) where

import Data.Aviation.Aip.Types.Aip as A
import Data.Aviation.Aip.Types.AipBook as A
import Data.Aviation.Aip.Types.AipBooks as A
import Data.Aviation.Aip.Types.AipBookTypes as A
import Data.Aviation.Aip.Types.AipChart as A
import Data.Aviation.Aip.Types.AipCharts as A
import Data.Aviation.Aip.Types.AipDAH as A
import Data.Aviation.Aip.Types.AipDAHs as A
import Data.Aviation.Aip.Types.AipDAP as A
import Data.Aviation.Aip.Types.AipDAPs as A
import Data.Aviation.Aip.Types.AipDate as A
import Data.Aviation.Aip.Types.AipERSA as A
import Data.Aviation.Aip.Types.AipERSAs as A
import Data.Aviation.Aip.Types.AipHref as A
import Data.Aviation.Aip.Types.AipPg as A
import Data.Aviation.Aip.Types.AipPrecisionObstacleChart as A
import Data.Aviation.Aip.Types.AipPrecisionObstacleCharts as A
import Data.Aviation.Aip.Types.AipSummarySUP_AIC as A
import Data.Aviation.Aip.Types.AipSummarySUP_AICs as A
import Data.Aviation.Aip.Types.AipSupplementsAIC as A
import Data.Aviation.Aip.Types.AipSupplementsAICs as A
import Data.Aviation.Aip.Types.Day as A
import Data.Aviation.Aip.Types.Link as A
import Data.Aviation.Aip.Types.Month as A
import Data.Aviation.Aip.Types.Year as A

{-}
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
-}