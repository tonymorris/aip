{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Aviation.Aip.Types.AipDate where

import Control.Applicative
import Control.Lens
import Data.Aviation.Aip.Types.Day
import Data.Aviation.Aip.Types.Month
import Data.Aviation.Aip.Types.Year
import Prelude
import Text.Parser.Char

data AipDate =
  AipDate {
    _aipday ::
      Day
  , _aipmonth ::
      Month
  , _aipyear ::
      Year
  } deriving (Eq, Ord, Show)

parseAipDate ::
  (CharParsing p, Monad p) =>
  p AipDate
parseAipDate =
  AipDate <$> parseDay <* char '-' <*> parseMonth <* char '-' <*> parseYear

makeClassy ''AipDate

instance HasDay AipDate where
  day =
    aipday . day
    
instance HasMonth AipDate where
  month =
    aipmonth . month

instance HasYear AipDate where
  year =
    aipyear . year

uriAipDate ::
  AipDate
  -> String
uriAipDate (AipDate (Day d1 d2) m (Year y1 y2 y3 y4)) =
  concat
    [
      show d1
    , show d2
    , "-"
    , show m
    , "-"
    , show y1
    , show y2
    , show y3
    , show y4
    ]

    