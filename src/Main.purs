module Main where

import Prelude
import Effect (Effect)
import Effect.Console (logShow)
import Loader(run)
import Data.Map(empty, lookup, insert, Map)
import Pux (start)
import Pux.Renderer.React (renderToDOM)
import Stories(view, foldp)

main :: Effect Unit
-- main = do
--   logShow run
main = do
  app <- start
    { initialState: { count: 0 }
    , view
    , foldp
    , inputs: []
    }
  renderToDOM "#app" app.markup app.input
  pure unit
