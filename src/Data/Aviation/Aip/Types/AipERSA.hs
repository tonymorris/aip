{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Aviation.Aip.Types.AipERSA where

import Control.Applicative
import Control.Lens
import Data.Aviation.Aip.Types.AipDate
import Data.Aviation.Aip.Types.AipHref
import Prelude

data AipERSA a =
  AipERSA {
    _aipersaname ::
      String
  , _aipersadate ::
      AipDate
  , _aipersahref ::
      AipHref
  , _aipersavalue ::
      a
  } deriving (Eq, Ord, Show)

instance Functor AipERSA where
  fmap f (AipERSA s d h a) =
    AipERSA s d h (f a)

instance Foldable AipERSA where
  foldr f z (AipERSA _ _ _ a) =
    f a z

instance Traversable AipERSA where
  traverse f (AipERSA s d h a) =
    AipERSA s d h <$> f a

makeClassy ''AipERSA
