module Data.Aviation.Aip.Types.AipBooks where

import Control.Applicative
import Control.Lens
import Data.Aviation.Aip.Types.AipBook
import Data.Aviation.Aip.Types.AipDate
import Data.Aviation.Aip.Types.AipHref
import Data.Digit
import Prelude
import Text.Parser.Char
import Text.Parser.Combinators

data AipBooks a =
  AipBooks
    [AipBook a]
  deriving (Eq, Ord, Show)

instance Functor AipBooks where
  fmap f (AipBooks x) =
    AipBooks ((f <$>) <$> x)
    
instance Foldable AipBooks where
  foldr f z (AipBooks x) =
    foldr (\a b -> foldr f b a) z x

instance Traversable AipBooks where
  traverse f (AipBooks x) =
    AipBooks <$> (traverse . traverse) f x

instance Monoid (AipBooks a) where
  AipBooks x `mappend` AipBooks y =
    AipBooks (x `mappend` y)
  mempty =
    AipBooks mempty
