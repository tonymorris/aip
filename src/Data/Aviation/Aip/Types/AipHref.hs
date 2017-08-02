module Data.Aviation.Aip.Types.AipHref where

import Data.Aviation.Aip.Types.AipDate
import Data.Aviation.Aip.Types.AipPg
import Data.Digit
import Prelude
import Text.Parser.Char

data AipHref =
  AipHref
    AipPg
    AipDate
    Digit
  deriving (Eq, Ord, Show)
  
parseAipHref ::
  (CharParsing p, Monad p) =>
  p AipHref
parseAipHref =
  string "aip.asp?pg=" *> 
  (AipHref <$> parseAipPg <* string "&vdate=" <*> parseAipDate <* string "&ver=" <*> parsedigit)
