module Noise where

import Prelude

import Effect
-- import Control.Monad.Eff (kind Effect, Eff)
-- import Control.Monad.Eff.Uncurried (EffFn1, EffFn3, runEffFn1, runEffFn3)

import Effect
import Effect.Uncurried
import Data.Function.Uncurried (Fn2, Fn3, runFn2, runFn3)


foreign import seedImpl :: EffectFn1 Number Void


seed :: Number -> Effect Void
seed = runEffectFn1 seedImpl

foreign import simplex2Impl :: EffectFn2 Number Number Number

simplex2 :: Number -> Number -> Effect Number
simplex2 = runEffectFn2 simplex2Impl
