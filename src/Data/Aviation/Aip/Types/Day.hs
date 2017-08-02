module Data.Aviation.Aip.Types.Day where

import Control.Applicative
import Control.Lens
import Data.Digit
import Prelude
import Text.Parser.Char
import Text.Parser.Combinators

data Day =
  Day
    Digit
    Digit
  deriving (Eq, Ord, Show)

parseDay ::
  (CharParsing p, Monad p) =>
  p Day
parseDay =
  Day <$> parsedigit <*> parsedigit
