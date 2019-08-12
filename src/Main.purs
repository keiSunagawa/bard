module Main where

import Prelude
import Effect (Effect)
import Effect.Console (logShow)
import Loader(run)
import Data.Map(empty, lookup, insert, Map)

main :: Effect Unit
main = do
  logShow run
