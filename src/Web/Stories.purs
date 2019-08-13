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
import Pux.Renderer.React (renderToDOM)
import Text.Smolder.HTML (button, div, form, input, span, h3)
import Text.Smolder.HTML.Attributes (name, type', value)
import Text.Smolder.Markup (text, (#!), (!), Markup)
import Data.Array (uncons)
import Data.Foldable (for_)

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
