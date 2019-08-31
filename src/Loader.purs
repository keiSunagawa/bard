module Loader
       ( run,
         ValueType,
         Template
       ) where

import Prelude
import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Foldable(foldl)
import Foreign (F, Foreign, ForeignError(..), fail, readArray, readString, isUndefined, readBoolean, readNullOrUndefined)
import Foreign.Index(readProp)
import Foreign.Keys(keys)
import Data.Function.Uncurried (Fn3, runFn3)
import Data.Bifunctor(lmap)
import Data.Map as M
import Data.Map(Map)
import Data.Foldable(foldl)
import Data.String(toLower)
import Data.Traversable(sequence)
import Bard(Story, Slip, Item, StoryRow, SlipColor(..))
import Data.Tuple(Tuple(..), fst)
import Data.Maybe(Maybe(..))

foreign import parseYAMLImpl :: forall r. Fn3 (String -> r) (Foreign -> r) String r

ferr = fail <<< ForeignError
-- | Attempt to parse a YAML string, returning the result as foreign data.
parseYAML :: String -> F Foreign
parseYAML yaml = runFn3 parseYAMLImpl ferr pure yaml

yamlToJson :: String -> Either String Foreign
yamlToJson s = case runExcept $ parseYAML s of
  Left err -> Left "Could not parse yaml"
  Right json -> Right json

decodeF :: Foreign -> Either String (Array String)
decodeF json =
  lmap show (runExcept $ ks)
  where
    ks = do
      alias <- readProp "alias" json
      keys alias

data ValueType = SingeValue | ArrayValue
instance showVT :: Show ValueType where
  show SingeValue = "SingleValue"
  show ArrayValue = "ArrayValue"

type Template = Array
                { key :: String
                , tpe :: ValueType
                , color :: SlipColor
                , optional :: Boolean}
type AliasMap = Map String String

foreignErrorToString :: forall a . F a -> Either String a
foreignErrorToString fa = lmap show (runExcept $ fa)

decodeTemplate :: Foreign -> Either String Template
decodeTemplate json =
  foreignErrorToString decoded
  where
    decoded = do
      tmp <- readProp "template" json
      xs <- readArray tmp
      sequence (decodeItem <$> xs)
    decodeItem itemJson =
      do
        label <- readProp "label" itemJson >>= readString
        tpeStr <- readProp "type" itemJson >>= readString
        tpe <- case toLower tpeStr of
          "single" -> pure SingeValue
          "array" -> pure ArrayValue
          utpe -> ferr $ "unknown type" <> utpe
        colorStr <- readProp "color" itemJson >>= readString
        color <- case toLower colorStr of
          "white" -> pure White
          "yellow" -> pure Yellow
          "blue" -> pure Blue
          "red" -> pure Red
          "green" -> pure Green
          otherwise -> ferr $ "unknown type" <> otherwise
        isOptional <- do
          m <- readProp "optional" itemJson >>= readNullOrUndefined
          case m of
            Nothing -> pure false
            Just v -> readBoolean v
        pure {key: label, tpe: tpe, color: color, optional: isOptional}

readMap :: forall a. Foreign -> (Foreign -> F a) -> F (Map String a)
readMap json f = do
  ks <- keys json
  xs <- sequence $ toKV <$> ks
  pure $ foldl (\m -> \kv -> M.insert kv.key kv.value m) M.empty xs
  where
    toKV key = readProp key json >>= f <#> \v -> {key: key, value: v}

decodeAlias :: Foreign -> Either String AliasMap
decodeAlias json =
  foreignErrorToString decoded
  where
    decoded = do
      als <- readProp "alias" json
      readMap als readString

decodeStory :: Foreign -> Template -> AliasMap -> Either String Story
decodeStory json tmp als = foreignErrorToString decodeStory'
  where
    getSlip :: Foreign -> F Slip
    getSlip js = do
      al <- readProp "alias" js >>= readMaybe readString
      v <- readProp "value" js >>= readMaybe readString
      ds <- readProp "disaibled" js >>= readMaybe readBoolean <#> \c -> case c of
        Just b -> b
        Nothing -> false
      value <- case Tuple al v of
        Tuple Nothing Nothing -> ferr "require alias or value."
        Tuple (Just _) (Just _) -> ferr "cannot use both."
        Tuple (Just al') Nothing -> case M.lookup al' als of
          Nothing -> ferr "alias not found."
          Just a -> pure a
        Tuple Nothing (Just v') -> pure v'
      pure { value: value, disaibled: ds }

    getItem :: Foreign -> ValueType -> F Item
    getItem js SingeValue = pure <$> getSlip js
    getItem js ArrayValue = do
      xs <- readArray js
      sequence $ getSlip <$> xs

    getStoryRaw :: Foreign -> F StoryRow
    getStoryRaw js = sequence $ tmp <#> \tp -> do
      itemJson <- readProp tp.key js
      mItemJson <- readNullOrUndefined itemJson
      case mItemJson of
        Nothing
          | tp.optional -> pure []
          | otherwise -> ferr $ "key '" <> tp.key <> "' is not optional."
        Just v -> getItem v tp.tpe

    decodeStory' = do
      storyJson <- readProp "story" json
      stories <- readArray storyJson
      xs <- sequence $ getStoryRaw <$> stories
      pure { labels: (\t -> {value: t.key, color: t.color}) <$> tmp, story: xs }

run :: String -> Either String Story
run yaml = do
  json <- yamlToJson yaml
  tmp <- decodeTemplate json
  als <- decodeAlias json
  decodeStory json tmp als

readMaybe :: forall a. (Foreign -> F a) -> Foreign -> F (Maybe a)
readMaybe f js = if isUndefined js
                      then pure Nothing
                      else Just <$> f js
