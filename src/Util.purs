module MyUtil
       ( undef
       , tearoff
       , unsafeLog
       ) where

import Data.Array(uncons, null)
import Data.Semigroup(class Semigroup, (<>))
import Data.Maybe(Maybe(..))
import Data.Function.Uncurried (Fn1, runFn1)

foreign import undefImpl :: forall a. a
undef = undefImpl

foreign import unsafeLogImpl :: forall a. Fn1 a a
unsafeLog :: forall a. a -> a
unsafeLog a = runFn1 unsafeLogImpl a

tearoff :: forall a b. Semigroup b => Array a -> (a -> b) -> Maybe b
tearoff xs headApFunc = case uncons xs of
  Just { head: h, tail: t } -> if null t
                                   then Just (headApFunc h)
                                   else Just (headApFunc h) <> tearoff t headApFunc
  Nothing -> Nothing
