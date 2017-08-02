module Data.Aviation.Aip.Types.Year where

import Data.Digit
import Prelude
import Text.Parser.Char

data Year =
  Year
    Digit
    Digit
    Digit
    Digit
  deriving (Eq, Ord, Show)

parseYear ::
  (CharParsing p, Monad p) =>
  p Year
parseYear =
  Year <$> parsedigit <*> parsedigit <*> parsedigit <*> parsedigit
