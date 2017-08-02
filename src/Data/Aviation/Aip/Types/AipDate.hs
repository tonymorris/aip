module Data.Aviation.Aip.Types.AipDate where

import Control.Applicative
import Data.Aviation.Aip.Types.Day
import Data.Aviation.Aip.Types.Month
import Data.Aviation.Aip.Types.Year
import Prelude
import Text.Parser.Char

data AipDate =
  AipDate
    Day
    Month
    Year
  deriving (Eq, Ord, Show)

parseAipDate ::
  (CharParsing p, Monad p) =>
  p AipDate
parseAipDate =
  AipDate <$> parseDay <* char '-' <*> parseMonth <* char '-' <*> parseYear
