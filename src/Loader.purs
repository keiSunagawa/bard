module Loader
       ( run,
         Hoge
       ) where

import Prelude
import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Foreign (F, Foreign, ForeignError(..), fail)
import Data.Function.Uncurried (Fn3, runFn3)
import Foreign.Generic(genericDecode)
import Foreign.Generic.Class(defaultOptions)

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
