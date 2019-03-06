module Diagram where

import Prelude

import Color (black)
import Color.Scale (sample)
import Color.Scale.Perceptual (magma)
import Data.Array ((..))
import Data.Foldable (fold)
import Data.Int (toNumber)
import Flare.Drawing (Drawing)
import Graphics.Drawing (closed, fillColor, filled, rotate, scale, shadow, shadowBlur, shadowColor, translate)
import Math (sin, cos, pi)

sierpinsky :: Int -> Int -> Number -> Drawing
sierpinsky points depth sizeScale = do
  shadow (shadowColor black <> shadowBlur 5.0) $
    translate 400.0 400.0 $
      scale 200.0 200.0 $
        go depth
  where
    nPoints = toNumber points

    theta i = pi / (nPoints/2.0) * toNumber i

    go 0 = mempty
    go n =
      let dr = scale sizeScale sizeScale (go (n - 1))
          colorSample = 1.0 - toNumber (n - 1) / nPoints
          repeat = do
            i <- 0..(points - 1)
            pure $ rotate (theta i)
                 $ translate 0.0 (cos (pi / nPoints) * (1.0 + sizeScale))
              -- $ translate (sin (pi / nPoints) * (1.0 + sizeScale)) (cos (pi / nPoints) * (1.0 + sizeScale))
                 $ dr

      in filled (fillColor (sample magma colorSample)) (closed mkShape)
         <> fold repeat

    mkShape = do
      i <- theta <$> 0..points
      pure { x: sin i, y: cos i }
