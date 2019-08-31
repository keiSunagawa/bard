module Bard
       (
         Slip,
         Item,
         StoryRow,
         Story,
         Label,
         SlipColor,
         SlipColor(..)
       ) where

import Data.Array
import Data.Map

-- Story -> StoryRow -> Item -> Slip

type Slip = { value :: String
            , disaibled :: Boolean
            }

type Item = Array Slip

type StoryRow = Array Item
type Story = { labels :: Array Label, story :: Array StoryRow }

type Label = { value :: String, color :: SlipColor }

data SlipColor =
  White
  | Yellow -- #FFFFDD
  | Blue -- #BBFFFF
  | Red -- #FFABCE
  | Green -- #CCFFCC
  -- | Custam
