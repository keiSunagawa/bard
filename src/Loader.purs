module Loader
       ( run2,
         Hoge
       ) where

import Prelude
import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Foreign (F, Foreign, ForeignError(..), fail)
import Foreign.Index(readProp)
import Foreign.Keys(keys)
import Data.Function.Uncurried (Fn3, runFn3)
import Foreign.Generic(genericDecode, Options)
import Foreign.Generic.Class(defaultOptions)
import Data.Bifunctor(lmap)

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

run :: Either String Hoge
run = yamlToData yamlInput

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

run2 :: Either String (Array String)
run2 = do
  json <- yamlToJson yamlInputProd
  decodeF json
