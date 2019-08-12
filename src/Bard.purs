module Bard
       (
         Slip,
         Item,
         StoryRow
       ) where

import Prelude
import Data.Array

newtype Slip = Slip
               { value :: String
               , disaibled :: Boolean
               }
type Item = Array Slip
type StoryRow = Array Item
type Story = Array StoryRow
type Labels = Array String

ds :: String -> Slip
ds s = Slip { value: s, disaibled: false }

sampleStoryRow :: StoryRow
sampleStoryRow =
  [ [ds "Consumer"]
  , [ds "goto work"]
  , [ds "lide car", ds "handle car"]
  ]

sampleStory :: Story
sampleStory = [sampleStoryRow, sampleStoryRow]

sampleLabels :: Labels
sampleLabels = ["Actor", "Narrative", "Feature"]
