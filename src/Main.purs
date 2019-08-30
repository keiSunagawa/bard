module Main where

import Prelude
import Effect (Effect)
import Pux (start)
import Pux.Renderer.React (renderToDOM)
import Stories(view, foldp, initState)

main :: Effect Unit
-- main = do
--   logShow run
main = do
  app <- start
    { initialState: initState
    , view
    , foldp
    , inputs: []
    }
  renderToDOM "#app" app.markup app.input
  pure unit
