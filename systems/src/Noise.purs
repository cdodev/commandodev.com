module Noise (seed, simplex2, simplex3) where

import Prelude
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, runEffectFn1, runEffectFn2, runEffectFn3)


foreign import seedImpl :: EffectFn1 Number Void

seed :: Number -> Effect Void
seed = runEffectFn1 seedImpl

foreign import simplex2Impl :: EffectFn2 Number Number Number

simplex2 :: Number -> Number -> Effect Number
simplex2 = runEffectFn2 simplex2Impl

foreign import simplex3Impl :: EffectFn3 Number Number Number Number

simplex3 :: Number -> Number -> Number -> Effect Number
simplex3 = runEffectFn3 simplex3Impl
