module Data.Aviation.Aip.Types.AipSupplementsAICs where

import Control.Applicative
import Control.Lens
import Data.Aviation.Aip.Types.AipPg
import Data.Aviation.Aip.Types.AipSupplementsAIC
import Data.Digit
import Prelude
import Text.Parser.Char
import Text.Parser.Combinators

data AipSupplementsAICs a =
  AipSupplementsAICs
    [AipSupplementsAIC a]
  deriving (Eq, Ord, Show)

instance Functor AipSupplementsAICs where
  fmap f (AipSupplementsAICs x) =
    AipSupplementsAICs ((f <$>) <$> x)
    
instance Foldable AipSupplementsAICs where
  foldr f z (AipSupplementsAICs x) =
    foldr (\a b -> foldr f b a) z x

instance Traversable AipSupplementsAICs where
  traverse f (AipSupplementsAICs x) =
    AipSupplementsAICs <$> (traverse . traverse) f x

instance Monoid (AipSupplementsAICs a) where
  AipSupplementsAICs x `mappend` AipSupplementsAICs y =
    AipSupplementsAICs (x `mappend` y)
  mempty =
    AipSupplementsAICs mempty
