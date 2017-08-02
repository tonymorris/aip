{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Aviation.Aip.Types.Day where

import Control.Applicative
import Control.Lens
import Data.Digit
import Prelude
import Text.Parser.Char

data Day =
  Day {
    _day1 ::
      Digit
  , _day2 ::
      Digit
  } deriving (Eq, Ord, Show)

parseDay ::
  (CharParsing p, Monad p) =>
  p Day
parseDay =
  Day <$> parsedigit <*> parsedigit

makeClassy ''Day
