{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Aviation.Aip.Types.AipChart where

import Control.Applicative
import Control.Lens
import Data.Aviation.Aip.Types.AipDate
import Data.Aviation.Aip.Types.AipHref
import Prelude

data AipChart a =
  AipChart {
    _aipchartname ::
      String
  , _aipchartdate ::
      AipDate
  , _aipcharthref ::
      AipHref
  , _aipchartvalue ::
      a
  }
  deriving (Eq, Ord, Show)

instance Functor AipChart where
  fmap f (AipChart s d h a) =
    AipChart s d h (f a)

instance Foldable AipChart where
  foldr f z (AipChart _ _ _ a) =
    f a z

instance Traversable AipChart where
  traverse f (AipChart s d h a) =
    AipChart s d h <$> f a

makeClassy ''AipChart
