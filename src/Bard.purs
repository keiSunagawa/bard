module Bard
       (
         Slip,
         Item,
         StoryRow,
         Story
       ) where

import Data.Array

type Slip = { value :: String
            , disaibled :: Boolean
            }
type Item = Array Slip
type StoryRow = Array Item
type Story = { labels :: Array String, story :: Array StoryRow }
type Labels = Array String
