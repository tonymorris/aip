{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Aviation.Aip.Types.AipHref where

import Control.Lens
import Data.Aviation.Aip.Types.AipDate
import Data.Aviation.Aip.Types.AipPg
import Data.Aviation.Aip.Types.Day
import Data.Aviation.Aip.Types.Month
import Data.Aviation.Aip.Types.Year
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

uriAipHref ::
  HasAipHref s =>
  s
  -> String
uriAipHref ahref =
  concat
    [
      "?pg="
    , show (ahref ^. aipHref . aippg1)
    , show (ahref ^. aipHref . aippg2)
    , "&vdate="
    , show (ahref ^. aipHref . day1)
    , show (ahref ^. aipHref . day2)
    , "-"
    , show (ahref ^. aipHref . month)
    , "-"
    , show (ahref ^. aipHref . year1)
    , show (ahref ^. aipHref . year2)
    , show (ahref ^. aipHref . year3)
    , show (ahref ^. aipHref . year4)
    , "&ver="
    , show (ahref ^. aipHref . hasdigit)
    ]

instance HasAipPg AipHref where
  aipPg =
    aiphrefpg . aipPg
    
instance HasAipDate AipHref where
  aipDate =
    aiphrefdate . aipDate
    
instance HasDigit AipHref where
  hasdigit =
    aiphrefversion . hasdigit
    
instance HasDay AipHref where
  day =
    aipDate . day
    
instance HasMonth AipHref where
  month =
    aipDate . month

instance HasYear AipHref where
  year =
    aipDate . year
    