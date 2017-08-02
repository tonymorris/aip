{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Aviation.Aip.Types.AipDAP where

import Control.Applicative
import Control.Lens
import Data.Aviation.Aip.Types.AipDate
import Data.Aviation.Aip.Types.AipHref
import Prelude

data AipDAP a =
  AipDAP {
    _aipdapname ::
      String
  , _aipdapdate ::
      AipDate
  , _aipdaphref ::
      AipHref
  , _aipdapvalue ::
      a
  }
  deriving (Eq, Ord, Show)

instance Functor AipDAP where
  fmap f (AipDAP s d h a) =
    AipDAP s d h (f a)

instance Foldable AipDAP where
  foldr f z (AipDAP _ _ _ a) =
    f a z

instance Traversable AipDAP where
  traverse f (AipDAP s d h a) =
    AipDAP s d h <$> f a

makeClassy ''AipDAP
