module Main where

import Prelude
import Effect (Effect)
import Effect.Console (logShow)
import Loader(run2)
main :: Effect Unit
main = do
  logShow run2
