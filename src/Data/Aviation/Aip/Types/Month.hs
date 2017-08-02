module Data.Aviation.Aip.Types.Month where

import Data.Digit
import Prelude
import Text.Parser.Combinators
import Text.Parser.Char

data Month =
  Jan
  | Feb
  | Mar
  | Apr
  | May
  | Jun
  | Jul
  | Aug
  | Sep
  | Oct
  | Nov
  | Dec
  deriving (Eq, Ord, Show)

parseMonth ::
  CharParsing p =>
  p Month
parseMonth =
  choice
    [
      Jan <$ string "Jan"
    , Feb <$ try (string "Feb")
    , Mar <$ try (string "Mar")
    , Apr <$ try (string "Apr")
    , May <$ try (string "May")
    , Jun <$ try (string "Jun")
    , Jul <$ try (string "Jul")
    , Aug <$ try (string "Aug")
    , Sep <$ try (string "Sep")
    , Oct <$ try (string "Oct")
    , Nov <$ try (string "Nov")
    , Dec <$ try (string "Dec")
    ]
