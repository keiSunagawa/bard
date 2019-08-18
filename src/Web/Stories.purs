module Stories
       (
         view,
         foldp,
         Event,
         State
       ) where

import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Pux (EffModel, noEffects, onlyEffects, start)
import Pux.DOM.Events (DOMEvent, onChange, onClick, onSubmit, targetValue)
import Pux.DOM.HTML (HTML)
import Pux.DOM.HTML.Attributes(style)
import Pux.Renderer.React (renderToDOM)
import Text.Smolder.HTML ( h1, button, div, form, input, span, h3)
import Text.Smolder.HTML.Attributes (name, type', value)
import Text.Smolder.Markup (text, (#!), (!), Markup)
import Data.Array (uncons)
import Data.Foldable (for_)
import CSS (color, fontSize, fontWeight, marginTop, lighter, rgb, em, px, border, solid, black, height, width, fromString)
import Data.Function (($), (#))

import CSS.Stylesheet (CSS, key)

data Event = Increment
           | Decrement

type State = { count :: Int}


-- | Return a new state (and effects) from each event
foldp :: Event -> State -> EffModel State Event
foldp Increment s = { state: s { count = s.count + 1 }, effects: [] }
foldp Decrement s = { state: s { count = s.count - 1 }, effects: [] }

-- | Return markup from the state
view :: State -> HTML Event
view state =
  div do
    h3 $ text "Youtube Subtitle Getter"
    button #! onClick (const Increment) $ text "Increment"
    span $ text (show state.count)
    button #! onClick (const Decrement) $ text "Decrement"
    box state

box :: forall ev st. st -> HTML ev
box state =
  div ! style do
    border solid (1.0 # px) black
    width (150.0 # px)
    height (150.0 # px)
    color (rgb 66 66 84)
    fontSize (2.0 # px)
    fontWeight lighter
    marginTop (0.0 # px)
    brakeAll
  $ text "Styled header"


brakeAll :: CSS
brakeAll = (key $ fromString "word-break") "break-all"
