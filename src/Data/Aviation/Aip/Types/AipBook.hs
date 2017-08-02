module Data.Aviation.Aip.Types.AipBook where

import Control.Applicative
import Control.Lens
import Data.Aviation.Aip.Types.AipDate
import Data.Aviation.Aip.Types.AipHref
import Prelude

data AipBook a =
  AipBook
    String
    AipDate
    AipHref
    a
  deriving (Eq, Ord, Show)

instance Functor AipBook where
  fmap f (AipBook s d h a) =
    AipBook s d h (f a)

instance Foldable AipBook where
  foldr f z (AipBook _ _ _ a) =
    f a z

instance Traversable AipBook where
  traverse f (AipBook s d h a) =
    AipBook s d h <$> f a
