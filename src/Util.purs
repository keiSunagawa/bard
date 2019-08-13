module MyUtil
       ( undef
       ) where

foreign import undefImpl :: forall a. a
undef = undefImpl
