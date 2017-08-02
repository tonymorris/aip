module Data.Aviation.Aip.Types.AipERSAs where

import Control.Applicative
import Control.Lens
import Data.Aviation.Aip.Types.AipERSA
import Prelude

data AipERSAs a =
  AipERSAs
    [AipERSA a]
  deriving (Eq, Ord, Show)

instance Functor AipERSAs where
  fmap f (AipERSAs x) =
    AipERSAs ((f <$>) <$> x)
    
instance Foldable AipERSAs where
  foldr f z (AipERSAs x) =
    foldr (\a b -> foldr f b a) z x

instance Traversable AipERSAs where
  traverse f (AipERSAs x) =
    AipERSAs <$> (traverse . traverse) f x

instance Monoid (AipERSAs a) where
  AipERSAs x `mappend` AipERSAs y =
    AipERSAs (x `mappend` y)
  mempty =
    AipERSAs mempty
