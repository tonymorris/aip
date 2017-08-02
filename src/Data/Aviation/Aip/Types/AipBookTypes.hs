module Data.Aviation.Aip.Types.AipBookTypes where

import Control.Applicative
import Control.Lens
import Data.Digit
import Prelude
import Text.Parser.Char
import Text.Parser.Combinators

data AipBookTypes =
  AipBookTypes {
    _bookcomplete ::
      String
  , _bookgeneral ::
      String
  , _bookenroute ::
      String
  , _bookaerodrome ::
      String
  , _bookindex ::
      String
  , _bookamendmentinstructions ::
      String
  } deriving (Eq, Ord, Show)
