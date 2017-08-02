module Data.Aviation.Aip.Types.AipDAHs where

import Control.Applicative
import Control.Lens
import Data.Aviation.Aip.Types.AipDAH
import Prelude

data AipDAHs a =
  AipDAHs
    [AipDAH a]
  deriving (Eq, Ord, Show)

instance Functor AipDAHs where
  fmap f (AipDAHs x) =
    AipDAHs ((f <$>) <$> x)
    
instance Foldable AipDAHs where
  foldr f z (AipDAHs x) =
    foldr (\a b -> foldr f b a) z x

instance Traversable AipDAHs where
  traverse f (AipDAHs x) =
    AipDAHs <$> (traverse . traverse) f x

instance Monoid (AipDAHs a) where
  AipDAHs x `mappend` AipDAHs y =
    AipDAHs (x `mappend` y)
  mempty =
    AipDAHs mempty
