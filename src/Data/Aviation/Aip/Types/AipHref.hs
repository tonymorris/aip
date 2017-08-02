{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Aviation.Aip.Types.AipHref where

import Control.Lens
import Data.Aviation.Aip.Types.AipDate
import Data.Aviation.Aip.Types.AipPg
import Data.Digit
import Prelude
import Text.Parser.Char

data AipHref =
  AipHref {
    _aiphrefpg ::
       AipPg
  , _aiphrefdate ::
       AipDate
  , _aiphrefversion ::
       Digit
  } deriving (Eq, Ord, Show)
  
parseAipHref ::
  (CharParsing p, Monad p) =>
  p AipHref
parseAipHref =
  string "aip.asp?pg=" *> 
  (AipHref <$> parseAipPg <* string "&vdate=" <*> parseAipDate <* string "&ver=" <*> parsedigit)

makeClassy ''AipHref
