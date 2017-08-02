{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Aviation.Aip.Types.AipPg where

import Control.Applicative
import Control.Lens
import Data.Digit
import Prelude
import Text.Parser.Char

data AipPg =
  AipPg {
    aippg1 ::
      Digit
  , aippg2 ::
      Digit
  } deriving (Eq, Ord, Show)

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

makeClassy ''AipPg
