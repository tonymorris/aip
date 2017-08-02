{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Aviation.Aip.Types.AipBookTypes where

import Control.Lens
import Prelude

data AipBookTypes =
  AipBookTypes {
    _bookcomplete ::
      String
  , _bookgeneral ::
      String
  , _bookenroute ::
      String
  , _bookaerodrome ::
      String
  , _bookindex ::
      String
  , _bookamendmentinstructions ::
      String
  } deriving (Eq, Ord, Show)

makeClassy ''AipBookTypes
