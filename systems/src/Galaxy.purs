module Galaxy where

import Prelude

import Color (hsla, cssStringHSLA, rgba, black)
import Control.Monad.ST (ST)
import Control.Monad.ST (run) as ST
import Data.Int (toNumber)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Random (random)
import Field (Acceleration, Config, PState, Particle, ParticleT, Radians(..), Vec, VecT, addTo, calcExtents, calculateField, colorConf, drawBackground, drawParticles, getState, incrHue, incrNoise, move, set, setAngle, setLength, setUp, wrap)
import Graphics.Canvas (Context2D, Dimensions, arc, fillPath, setFillStyle)
import Math as M
import Noise (seed, simplex3)
import Record.ST (STRecord)
import Record.ST (freeze) as ST
import Unsafe.Coerce (unsafeCoerce)
import Web.HTML (window)
import Web.HTML.Window (requestAnimationFrame)


calcField :: forall r. Config -> PState -> (Int -> Int -> Vec -> ST r Unit)
calcField cnf st =
  let dims = cnf.fieldDim
      z = toNumber cnf.zoom
      toRec :: Acceleration -> STRecord r VecT
      toRec = unsafeCoerce
      fn :: Int -> Int -> Acceleration -> ST r Unit
      fn col row accV' = do

        let accV = toRec accV'
            x' = toNumber col
            y' = toNumber row
            dx = x'-(toNumber dims.cols)/2.0
            dy = y'-(toNumber dims.rows)/2.0
            a  = Radians $ (M.atan2 dy dx) + M.pi/2.0
            l = (M.sqrt (dx*dx + dy*dy))/100.0
            x1 = (_ / 2.0) $ simplex3 (x'/z) (y'/z) st.noiseZ
            y1 = (_ / 2.0) $ simplex3 (x'/z + 40000.0) (y'/z + 40000.0) st.noiseZ
        setAngle accV a
        setLength accV l
        addTo accV { x: x1, y: y1}
  in fn


        --   angle = (_ * M.pi * 2.0) $ (_ / 2.0) $ simplex3 (x'/z) (y'/z) st.noiseZ
        --   length = (_ / 2.0) $ simplex3 (x'/z*2.0 + 40000.0) (y'/z*2.0 + 40000.0) st.noiseZ
        --   newVec = toRec {x: 0.0, y:length}
        -- setAngle newVec $ Radians angle
        -- v <- ST.freeze newVec
        -- set accV v


drawAll :: Context2D -> Config -> PState -> Effect Unit
drawAll ctx conf ps = do
  let hueC = ps.hueCounter
      h = (M.sin hueC * (toNumber conf.cc.hueRange)) + toNumber conf.cc.baseHue
      c = hsla h (toNumber conf.cc.sat) 0.5 conf.cc.opacity
  setFillStyle ctx $ cssStringHSLA c
  drawParticles conf.fieldSize conf.fieldDim f
  where
    f p a = do
      let _ = ST.run (doMove p a)
      drawParticle ctx ps conf p
    doMove :: forall r. Particle -> Acceleration -> ST r Unit
    doMove p' a = do
      let p = toRec p'
      move p a conf.particleSpeed
      wrap p conf.canvasDim
    toRec :: forall r. Particle -> STRecord r ParticleT
    toRec = unsafeCoerce

drawParticle :: Context2D -> PState -> Config -> Particle -> Effect Unit
drawParticle ctx ps conf p = do
  -- beginPath ctx
  fillPath ctx $ arc ctx p'
  where
    p' = { x: p.pos.x, y: p.pos.y, radius: p.size, start: 0.0, end: 2.0*M.pi}

draw :: Context2D -> Config -> Effect Unit
draw ctx conf = do
  state <- getState
  drawBackground ctx conf.canvasDim $ rgba 0 0 0 0.1
  void $ requestAnimationFrame (draw ctx conf) =<< window
  let dims = conf.fieldDim
  calculateField dims (\c r v -> ST.run (calcField conf state c r v))
  drawAll ctx conf state
  incrNoise conf.noiseSpeed
  incrHue conf.cc.hueSpeed

--------------------------------------------------------------------------------
-- SET UP AND MAIN

mkConfig :: Int -> Dimensions -> Config
mkConfig fSize dim =
  { zoom: 80
  , fieldDim: calcExtents dim fSize
  , canvasDim: dim
  , noiseSpeed: 0.01
  , particleSpeed: 1.0
  , fieldForce: 90
  , fieldSize: fSize
  , randomForce: 10
  , cc: colorConf { hueSpeed = 0.005 }
  }


main :: Effect Unit
main = do
  state <- getState
  _ <- seed =<< random

  (Tuple ctx conf) <- setUp "canvas" $ mkConfig 5
  drawBackground ctx conf.canvasDim black
  draw ctx conf
