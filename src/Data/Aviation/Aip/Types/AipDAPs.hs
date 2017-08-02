{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Aviation.Aip.Types.AipDAPs where

import Control.Applicative
import Control.Lens
import Data.Aviation.Aip.Types.AipDAP
import Prelude

data AipDAPs a =
  AipDAPs
    [AipDAP a]
  deriving (Eq, Ord, Show)

instance Functor AipDAPs where
  fmap f (AipDAPs x) =
    AipDAPs ((f <$>) <$> x)
    
instance Foldable AipDAPs where
  foldr f z (AipDAPs x) =
    foldr (\a b -> foldr f b a) z x

instance Traversable AipDAPs where
  traverse f (AipDAPs x) =
    AipDAPs <$> (traverse . traverse) f x

instance Monoid (AipDAPs a) where
  AipDAPs x `mappend` AipDAPs y =
    AipDAPs (x `mappend` y)
  mempty =
    AipDAPs mempty

makeWrapped ''AipDAPs
