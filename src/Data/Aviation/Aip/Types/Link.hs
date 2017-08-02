{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Aviation.Aip.Types.Link where

import Control.Lens
import Prelude

data Link =
  Link
    String
  deriving (Eq, Ord, Show)

makeWrapped ''Link
