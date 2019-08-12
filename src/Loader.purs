module Loader
       ( run,
         ValueType,
         Template
       ) where

import Prelude
import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Foreign (F, Foreign, ForeignError(..), fail, readArray, readString)
import Foreign.Index(readProp)
import Foreign.Keys(keys)
import Data.Function.Uncurried (Fn3, runFn3)
import Foreign.Generic(genericDecode, Options)
import Foreign.Generic.Class(defaultOptions)
import Data.Bifunctor(lmap)
import Data.Map(empty, lookup, insert, Map)
import Data.Foldable(foldl)
import Data.String(toLower)
import Data.Traversable(sequence)

foreign import parseYAMLImpl :: forall r. Fn3 (String -> r) (Foreign -> r) String r

-- | Attempt to parse a YAML string, returning the result as foreign data.
parseYAML :: String -> F Foreign
parseYAML yaml = runFn3 parseYAMLImpl (fail <<< ForeignError) pure yaml

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
      alisa: cs
    flow:
      value: "goto work"
    feature:
      - value: "lide car"
        disaibled: true
      - value: "handle car"
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
      pure $ foldl (\m -> \di -> insert di.label di.tpe m) empty xs'
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
  pure $ foldl (\m -> \kv -> insert kv.key kv.value m) empty xs
  where
    toKV key = readProp key json >>= f <#> \v -> {key: key, value: v}

decodeAlias :: Foreign -> Either String AliasMap
decodeAlias json =
  foreignErrorToString decoded
  where
    decoded = do
      als <- readProp "alias" json
      readMap als readString

run2 :: Either String Template
run2 = do
  json <- yamlToJson yamlInputProd
  decodeTemplate json

run = do
  json <- yamlToJson yamlInputProd
  tmp <- decodeTemplate json
  als <- decodeAlias json
  pure {als: als, tmp: tmp}
