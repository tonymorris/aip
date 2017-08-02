module Data.Aviation.Aip.Types.AipCharts where

import Control.Applicative
import Control.Lens
import Data.Aviation.Aip.Types.AipChart
import Prelude

data AipCharts a =
  AipCharts
    [AipChart a]
  deriving (Eq, Ord, Show)

instance Functor AipCharts where
  fmap f (AipCharts x) =
    AipCharts ((f <$>) <$> x)
    
instance Foldable AipCharts where
  foldr f z (AipCharts x) =
    foldr (\a b -> foldr f b a) z x

instance Traversable AipCharts where
  traverse f (AipCharts x) =
    AipCharts <$> (traverse . traverse) f x

instance Monoid (AipCharts a) where
  AipCharts x `mappend` AipCharts y =
    AipCharts (x `mappend` y)
  mempty =
    AipCharts mempty
    