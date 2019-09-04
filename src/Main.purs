module Main where

import Prelude

import Effect (Effect)
import Effect.Console (logShow)

-- import Pux (start)
-- import Pux.Renderer.React (renderToDOM)
-- import Stories(view, foldp, initState)

main :: Effect Unit
main = do
  logShow "hoge"
-- main = do
--   app <- start
--     { initialState: initState
--     , view
--     , foldp
--     , inputs: []
--     }
--   renderToDOM "#app" app.markup app.input
--   pure unit
