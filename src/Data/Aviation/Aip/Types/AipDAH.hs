module Data.Aviation.Aip.Types.AipDAH where

import Control.Applicative
import Control.Lens
import Data.Digit
import Prelude
import Text.Parser.Char
import Text.Parser.Combinators

data AipDAH a =
  AipDAH
    String
    String
    a
  deriving (Eq, Ord, Show)

instance Functor AipDAH where
  fmap f (AipDAH s1 s2 a) =
    AipDAH s1 s2 (f a)
    
instance Foldable AipDAH where
  foldr f z (AipDAH _ _ a) =
    f a z

instance Traversable AipDAH where
  traverse f (AipDAH s1 s2 a) =
    AipDAH s1 s2 <$> f a
