module Data.Aviation.Aip.Types.AipSupplementsAIC where

import Control.Applicative
import Control.Lens
import Data.Aviation.Aip.Types.AipPg
import Prelude

data AipSupplementsAIC a =
  AipSupplementsAIC
    String
    AipPg
    a
  deriving (Eq, Ord, Show)

instance Functor AipSupplementsAIC where
  fmap f (AipSupplementsAIC s p a) =
    AipSupplementsAIC s p (f a)

instance Foldable AipSupplementsAIC where
  foldr f z (AipSupplementsAIC _ _ a) =
    f a z

instance Traversable AipSupplementsAIC where
  traverse f (AipSupplementsAIC s p a) =
    AipSupplementsAIC s p <$> f a
