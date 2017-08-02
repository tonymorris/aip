{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Aviation.Aip.Types.AipPrecisionObstacleCharts where

import Control.Applicative
import Control.Lens
import Data.Aviation.Aip.Types.AipPrecisionObstacleChart
import Prelude

data AipPrecisionObstacleCharts a =
  AipPrecisionObstacleCharts
    [AipPrecisionObstacleChart a]
  deriving (Eq, Ord, Show)

instance Functor AipPrecisionObstacleCharts where
  fmap f (AipPrecisionObstacleCharts x) =
    AipPrecisionObstacleCharts ((f <$>) <$> x)
    
instance Foldable AipPrecisionObstacleCharts where
  foldr f z (AipPrecisionObstacleCharts x) =
    foldr (\a b -> foldr f b a) z x

instance Traversable AipPrecisionObstacleCharts where
  traverse f (AipPrecisionObstacleCharts x) =
    AipPrecisionObstacleCharts <$> (traverse . traverse) f x

instance Monoid (AipPrecisionObstacleCharts a) where
  AipPrecisionObstacleCharts x `mappend` AipPrecisionObstacleCharts y =
    AipPrecisionObstacleCharts (x `mappend` y)
  mempty =
    AipPrecisionObstacleCharts mempty

makeWrapped ''AipPrecisionObstacleCharts
