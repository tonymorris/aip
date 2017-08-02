module Data.Aviation.Aip.Types.AipSummarySUP_AICs where

import Control.Applicative
import Control.Lens
import Data.Aviation.Aip.Types.AipSummarySUP_AIC
import Prelude

data AipSummarySUP_AICs a =
  AipSummarySUP_AICs
    [AipSummarySUP_AIC a]
  deriving (Eq, Ord, Show)

instance Functor AipSummarySUP_AICs where
  fmap f (AipSummarySUP_AICs x) =
    AipSummarySUP_AICs ((f <$>) <$> x)

instance Foldable AipSummarySUP_AICs where
  foldr f z (AipSummarySUP_AICs x) =
    foldr (\a b -> foldr f b a) z x

instance Traversable AipSummarySUP_AICs where
  traverse f (AipSummarySUP_AICs x) =
    AipSummarySUP_AICs <$> (traverse . traverse) f x

instance Monoid (AipSummarySUP_AICs a) where
  AipSummarySUP_AICs x `mappend` AipSummarySUP_AICs y =
    AipSummarySUP_AICs (x `mappend` y)
  mempty =
    AipSummarySUP_AICs mempty
