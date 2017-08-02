{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Aviation.Aip.Types.Year where

import Control.Lens
import Data.Digit
import Prelude
import Text.Parser.Char

data Year =
  Year {
    _year1 ::
      Digit
  , _year2 ::
      Digit
  , _year3 ::
      Digit
  , _year4 ::
      Digit
  }
  deriving (Eq, Ord, Show)

parseYear ::
  (CharParsing p, Monad p) =>
  p Year
parseYear =
  Year <$> parsedigit <*> parsedigit <*> parsedigit <*> parsedigit

makeClassy ''Year
