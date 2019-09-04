module Stories(component) where

import Prelude

import Bard (Item, Label, SlipColor(..), Story)
import CSS (Color, Rule(..), background, black, border, color, fontSize, fontWeight, fromString, height,  lighter, marginTop, px, rgb, solid, width)
import CSS.Color (rgb)
import CSS.Stylesheet (CSS, key)
import CSS.VerticalAlign (verticalAlign, textTop)
import Control.Bind (discard)
import Control.Category (identity)
import Control.Semigroupoid ((<<<))
import Data.Array (zip)
import Data.Array as A
import Data.Either (Either(..))
import Data.Function (($), (#), const)
import Data.Functor ((<$>))
import Data.List (fromFoldable, transpose)
import Data.Maybe (Maybe(..))
import Data.Ring ((-))
import Data.Semiring ((+))
import Data.Show (show)
import Data.Tuple (Tuple(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HCS
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Loader (run)
import MyUtil (tearoff, undef)

type State =
  { storyYamlIn :: String }
data Action = InputYaml String

component :: forall m q. H.Component HH.HTML q Unit  Unit m
component =
  H.mkComponent
    { eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    , initialState
    , render
    }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
      HH.div_
        [ HH.h3_ [HH.text "User Story Mapping"]
        , inputBox state
        , case run state.storyYamlIn of
            Left s -> HH.div_ [HH.text s]
            Right s -> renderStory s
        ]

handleAction :: forall o m. Action ->  H.HalogenM State Action () o m Unit
handleAction = case _ of
  InputYaml ym ->
    H.modify_ \st -> st { storyYamlIn = ym }

inputBox :: forall s m. State -> H.ComponentHTML Action s m
inputBox state = HH.textarea
          [ HCS.style do
                        width (550.0 # px)
                        height (300.0 # px)
          , HP.value state.storyYamlIn
          , HE.onValueInput \v -> Just (InputYaml v)
          ]

slip :: forall s m. Color -> String -> H.ComponentHTML Action s m
slip cr str = HH.div
              [ HCS.style do
                            border solid (1.0 # px) black
                            width (150.0 # px)
                            height (150.0 # px)
                            color (rgb 66 66 84)
                            background cr
                            fontSize (2.0 # px)
                            fontWeight lighter
                            marginTop (0.0 # px)
                            brakeAll
              ]
              [ HH.text str ]

borderCollapse :: CSS
borderCollapse = (key $ fromString "border-collapse") "collapse"

brakeAll :: CSS
brakeAll = (key $ fromString "word-break") "break-all"

renderStory :: forall s m. Story -> H.ComponentHTML Action s m
renderStory s = HH.table [HCS.style borderCollapse] (renderRow <$> tbl)
  where
    tbl :: Array (Tuple Label (Array Item))
    tbl = zip s.labels (transposeArray s.story)

    renderRow :: Tuple Label (Array Item) -> H.ComponentHTML Action s m
    renderRow (Tuple h b) =
      HH.tr
       [ HCS.style (border solid (5.0 # px) black) ]
       ([ HH.th_ [ HH.text h.value ]] <> renderItems h.color b)

    renderItems :: SlipColor -> Array Item -> Array (H.ComponentHTML Action s m)
    renderItems c xs = case tearoff xs (\x -> [renderItem c x]) of
      Just m -> m
      Nothing -> [HH.td_ [ HH.text "no body" ]]

    renderItem :: SlipColor -> Item -> H.ComponentHTML Action s m
    renderItem c xs = case tearoff xs (\h -> [slip (renderColor c) h.value]) of
      Just m -> HH.td [ HCS.style (verticalAlign textTop) ] m
      Nothing -> HH.td_ [ HH.text "no body"]

renderColor :: SlipColor -> Color
renderColor White = rgb 255 255 255
renderColor Yellow = rgb 255 255 221
renderColor Blue = rgb 187 255 255
renderColor Red = rgb 255 171 206
renderColor Green = rgb 204 255 204

transposeArray :: forall a. Array (Array a) -> Array (Array a)
transposeArray xs = A.fromFoldable $ A.fromFoldable <$> lxs
  where
    lxs = (transpose <<< fromFoldable) $ fromFoldable <$> xs

-- ---

initialYaml = """
template:
  - label: user
    type: "single"
    color: Red
    optional: true
  - label: flow
    type: "single"
    color: Blue
  - label: feature
    type: "array"
    color: Green
alias:
  cs: Consumer
story:
  - user:
      alias: cs
    flow:
      value: "goto work"
    feature:
      - value: "get on car"
        disaibled: true
      - value: "handle car"
  - user:
      alias: cs
    flow:
      value: "pre work"
    feature:
      - value: "crean"
      - value: "power on"
"""

initialState :: Unit -> State
initialState _ = { storyYamlIn: initialYaml }
