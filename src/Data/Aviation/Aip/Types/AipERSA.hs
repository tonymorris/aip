module Data.Aviation.Aip.Types.AipERSA where

import Control.Applicative
import Control.Lens
import Data.Aviation.Aip.Types.AipDate
import Data.Aviation.Aip.Types.AipHref
import Data.Digit
import Prelude
import Text.Parser.Char
import Text.Parser.Combinators

data AipERSA a =
  AipERSA
    String
    AipDate
    AipHref
    a
  deriving (Eq, Ord, Show)

instance Functor AipERSA where
  fmap f (AipERSA s d h a) =
    AipERSA s d h (f a)

instance Foldable AipERSA where
  foldr f z (AipERSA _ _ _ a) =
    f a z

instance Traversable AipERSA where
  traverse f (AipERSA s d h a) =
    AipERSA s d h <$> f a

