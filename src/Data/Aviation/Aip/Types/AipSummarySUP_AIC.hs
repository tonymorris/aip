module Data.Aviation.Aip.Types.AipSummarySUP_AIC where

import Control.Applicative
import Control.Lens
import Prelude

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

instance Foldable AipSummarySUP_AIC where
  foldr f z (AipSummarySUP_AIC _ _ _ a) =
    f a z

instance Traversable AipSummarySUP_AIC where
  traverse f (AipSummarySUP_AIC s1 s2 s3 a) =
    AipSummarySUP_AIC s1 s2 s3 <$> f a
