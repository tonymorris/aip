{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Aviation.Aip.Types.AipPrecisionObstacleChart where

import Control.Applicative
import Control.Lens
import Prelude

data AipPrecisionObstacleChart a =
  AipPrecisionObstacleChart {
    _aipprecisionobstancechartname ::
      String
  , _aipprecisionobstancecharthref ::
      String
  , _aipprecisionobstancechartvalue ::
      a
  } deriving (Eq, Ord, Show)

instance Functor AipPrecisionObstacleChart where
  fmap f (AipPrecisionObstacleChart s1 s2 a) =
    AipPrecisionObstacleChart s1 s2 (f a)
    
instance Foldable AipPrecisionObstacleChart where
  foldr f z (AipPrecisionObstacleChart _ _ a) =
    f a z

instance Traversable AipPrecisionObstacleChart where
  traverse f (AipPrecisionObstacleChart s1 s2 a) =
    AipPrecisionObstacleChart s1 s2 <$> f a

makeClassy ''AipPrecisionObstacleChart
