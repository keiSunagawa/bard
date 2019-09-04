module Stories where
-- module Stories
--        (
--          view,
--          foldp,
--          Event,
--          State,
--          transposeArray,
--          initState
--        ) where

-- import Data.Show(show)
-- import Control.Category(identity)
-- import Control.Semigroupoid((<<<))
-- import Data.Semiring((+))
-- import Control.Bind(discard)
-- import Data.Ring((-))
-- import Data.Maybe (Maybe(..))
-- import Pux (EffModel)
-- import Pux.DOM.Events (onClick, onChange, targetValue)
-- import Pux.DOM.HTML (HTML)
-- import Pux.DOM.HTML.Attributes(style)
-- import Text.Smolder.HTML.Attributes(value)
-- import Text.Smolder.HTML (button, div, span, h3, table, th, td, tr, textarea)
-- import Text.Smolder.Markup (text, (#!), (!), Markup)
-- import Data.Array as A
-- import Data.List(fromFoldable, transpose)
-- import CSS (color, fontSize, fontWeight, marginTop, lighter, rgb, px, border, solid, black, height, width, fromString, azure, Color, background)
-- import CSS.VerticalAlign(verticalAlign, textTop)
-- import CSS.Color(rgb)
-- import Data.Function (($), (#), const)
-- import Data.Functor ((<$>))
-- import Bard(Story, Item, Label, SlipColor, SlipColor(..))
-- import MyUtil(tearoff)
-- import Data.Array(zip)
-- import Data.Tuple(Tuple(..))
-- import CSS.Stylesheet (CSS, key)
-- import Loader(run)
-- import Data.Either(Either(..))

-- data Event = InputYaml String

-- type State =
--   { storyYamlIn :: String }

-- -- | Return a new state (and effects) from each event
-- foldp :: Event -> State -> EffModel State Event
-- foldp (InputYaml inp) s = { state: s { storyYamlIn = inp }, effects: [] }

-- -- | Return markup from the state
-- view :: State -> HTML Event
-- view state =
--   div do
--     h3 $ text "User Story Mapping"
--     yamlInputBox state
--     case run state.storyYamlIn of
--       Left s -> div $ text s
--       Right s -> renderStory s

-- yamlInputBox :: State -> HTML Event
-- yamlInputBox st = textarea ! style do
--     width (550.0 # px)
--     height (300.0 # px)
--   ! value st.storyYamlIn #! onChange (\ev -> InputYaml (targetValue ev)) $ text ""

-- slip :: forall ev . Color -> String -> Markup ev
-- slip cr str  =
--   div ! style do
--     border solid (1.0 # px) black
--     width (150.0 # px)
--     height (150.0 # px)
--     color (rgb 66 66 84)
--     background cr
--     fontSize (2.0 # px)
--     fontWeight lighter
--     marginTop (0.0 # px)
--     brakeAll
--   $ text str

-- borderCollapse :: CSS
-- borderCollapse = (key $ fromString "border-collapse") "collapse"

-- brakeAll :: CSS
-- brakeAll = (key $ fromString "word-break") "break-all"

-- renderStory :: Story -> HTML Event
-- renderStory s = table ! style borderCollapse $ r $ renderRow <$> tbl
--   where
--     tbl :: Array (Tuple Label (Array Item))
--     tbl = zip s.labels (transposeArray s.story)

--     renderRow :: forall ev. Tuple Label (Array Item) -> Markup ev
--     renderRow (Tuple h b) = tr ! style (border solid (5.0 # px) black) $ do
--       th $ text h.value
--       renderItems h.color b

--     renderItems :: forall e. SlipColor -> Array Item -> Markup e
--     renderItems c xs = case tearoff xs (renderItem c) of
--       Just m -> m
--       Nothing -> td $ text "no body"

--     renderItem :: forall e. SlipColor -> Item -> Markup e
--     renderItem c xs = case tearoff xs (\h -> slip (renderColor c) h.value) of
--       Just m -> td ! style (verticalAlign textTop) $ m
--       Nothing -> td $ text "no body"

--     r :: forall a. Array (Markup a) -> Markup a
--     r xs = case tearoff xs identity of
--       Just x -> x
--       Nothing -> div $ text "no value."

-- renderColor :: SlipColor -> Color
-- renderColor White = rgb 255 255 255
-- renderColor Yellow = rgb 255 255 221
-- renderColor Blue = rgb 187 255 255
-- renderColor Red = rgb 255 171 206
-- renderColor Green = rgb 204 255 204

-- transposeArray :: forall a. Array (Array a) -> Array (Array a)
-- transposeArray xs = A.fromFoldable $ A.fromFoldable <$> lxs
--   where
--     lxs = (transpose <<< fromFoldable) $ fromFoldable <$> xs

-- ---

-- initYaml = """
-- template:
--   - label: user
--     type: "single"
--     color: Red
--     optional: true
--   - label: flow
--     type: "single"
--     color: Blue
--   - label: feature
--     type: "array"
--     color: Green
-- alias:
--   cs: Consumer
-- story:
--   - user:
--       alias: cs
--     flow:
--       value: "goto work"
--     feature:
--       - value: "lide car"
--         disaibled: true
--       - value: "handle car"
--   - user:
--       alias: cs
--     flow:
--       value: "pre work"
--     feature:
--       - value: "crean"
--       - value: "standing pc"
-- """

-- initState :: State
-- initState = { storyYamlIn: initYaml }
