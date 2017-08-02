module Data.Aviation.Aip.Types.AipPg where

import Control.Applicative
import Control.Lens
import Data.Digit
import Prelude
import Text.Parser.Char
import Text.Parser.Combinators

data AipPg =
  AipPg
    Digit
    Digit
  deriving (Eq, Ord, Show)

parseAipPg ::
  (CharParsing p, Monad p) =>
  p AipPg
parseAipPg =
  AipPg <$> parsedigit <*> parsedigit

parseAipPgHref ::
  (CharParsing p, Monad p) =>
  p AipPg
parseAipPgHref =
  string "aip.asp?pg=" *> 
  parseAipPg
