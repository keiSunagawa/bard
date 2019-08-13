module Loader
       ( run,
         ValueType,
         Template
       ) where

import Prelude
import Data.Array as A
import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Foreign (F, Foreign, ForeignError(..), fail, readArray, readString, isUndefined, readBoolean)
import Foreign.Index(readProp)
import Foreign.Keys(keys)
import Data.Function.Uncurried (Fn3, runFn3)
import Foreign.Generic(genericDecode, Options)
import Foreign.Generic.Class(defaultOptions)
import Data.Bifunctor(lmap)
import Data.Map as M
import Data.Map(Map)
import Data.Foldable(foldl)
import Data.String(toLower)
import Data.Traversable(sequence)
import MyUtil(undef)
import Bard(Story, Slip, Item, StoryRow)
import Data.Tuple(Tuple(..), fst)
import Data.Maybe(Maybe(..))

foreign import parseYAMLImpl :: forall r. Fn3 (String -> r) (Foreign -> r) String r

ferr = fail <<< ForeignError
-- | Attempt to parse a YAML string, returning the result as foreign data.
parseYAML :: String -> F Foreign
parseYAML yaml = runFn3 parseYAMLImpl ferr pure yaml

yamlInput :: String
yamlInput = """
hoge:
  fuga: "aaa"
  piyo: "bbb"
"""

-- needs decode from foreign json
opts :: Options
opts = defaultOptions { unwrapSingleConstructors = true }

yamlToData :: String -> Either String Hoge
yamlToData s = case runExcept $ parseYAML s of
  Left err -> Left "Could not parse yaml"
  Right json -> case runExcept $ genericDecode opts json of
    Left e -> Left $ show e
    Right h -> Right h

newtype Hoge = Hoge
               { hoge :: {
                    fuga :: String
                    , piyo :: String
                    }
               }
derive instance genericHoge :: Generic Hoge _
instance showMyRecord :: Show Hoge where show = genericShow

yamlInputProd :: String
yamlInputProd = """
template:
  - label: user
    type: "single"
  - label: flow
    type: "single"
  - label: feature
    type: "array"
alias:
  cs: Consumer
story:
  - user:
      alias: cs
    flow:
      value: "goto work"
    feature:
      - value: "lide car"
        disaibled: true
      - value: "handle car"
  - user:
      alias: cs
    flow:
      value: "pre work"
    feature:
      - value: "crean"
      - value: "standing pc"
"""

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

type Template = Map String ValueType
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
      xs' <- sequence (decodeItem <$> xs)
      pure $ foldl (\m -> \di -> M.insert di.label di.tpe m) M.empty xs'
    decodeItem itemJson =
      do
        label <- readProp "label" itemJson >>= readString
        tpeStr <- readProp "type" itemJson >>= readString
        tpe <- case toLower tpeStr of
          "single" -> pure SingeValue
          "array" -> pure ArrayValue
          utpe -> fail $ ForeignError $ "unknown type" <> utpe
        pure {label: label, tpe: tpe}

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
    tps :: Array (Tuple String ValueType)
    tps = M.toUnfoldable tmp

    getStoryRaw :: Foreign -> F StoryRow
    getStoryRaw js = do
      sequence $ tps <#> \tp -> case tp of
        Tuple key vt -> do
          itemJson <- readProp key js
          getItem itemJson vt

    getItem :: Foreign -> ValueType -> F Item
    getItem js SingeValue = pure <$> getSlip js
    getItem js ArrayValue = do
      xs <- readArray js
      sequence $ getSlip <$> xs

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
    readMaybe :: forall a. (Foreign -> F a) -> Foreign -> F (Maybe a)
    readMaybe f js = if isUndefined js
                            then pure Nothing
                            else Just <$> f js
    decodeStory' = do
      storyJson <- readProp "story" json
      stories <- readArray storyJson
      xs <- sequence $ getStoryRaw <$> stories
      pure { labels: fst <$> tps, story: xs }

run = do
  json <- yamlToJson yamlInputProd
  tmp <- decodeTemplate json
  als <- decodeAlias json
  decodeStory json tmp als
