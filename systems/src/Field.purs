module Field where

import Prelude

import Data.Tuple (Tuple(..))
import Control.Monad.ST (ST)
import Color (Color, cssStringRGBA)
import Data.Array ((..))
import Data.Function.Uncurried (Fn1, Fn2, Fn3, mkFn3, runFn1, runFn2, runFn3)
import Data.Int (toNumber, floor)
import Data.Maybe (maybe)
import Data.Newtype (class Newtype)
import Data.Traversable (for)
import Effect (Effect)
import Effect.Exception (throw)
import Effect.Random (random)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, mkEffectFn2, runEffectFn1, runEffectFn2, runEffectFn3)
import Graphics.Canvas (Context2D, Dimensions, getCanvasDimensions, getCanvasElementById, getContext2D)
import Record.ST (STRecord)


--------------------------------------------------------------------------------
-- TYPES

type VecT = ( x :: Number, y :: Number )

type Vec = Record VecT

type Acceleration = Vec
type Velocity = Vec
type XY = Vec

type FieldDim = { cols :: Int, rows :: Int }

type ParticleT = (
    pos :: XY
  , prevPos :: XY
  , vel :: Velocity
  , acc :: Acceleration
  , size :: Number
  )

type ColorConfig = {
    opacity :: Number
  , baseHue :: Int
  , hueRange :: Int
  , hueSpeed :: Number
  , sat :: Int
  }

colorConf :: ColorConfig
colorConf = { opacity: 0.7
            , baseHue: 270
            , hueRange: 90
            , hueSpeed: 0.01
            , sat: 50
            }

type Config = {
    zoom :: Int
  , fieldDim :: FieldDim
  , canvasDim :: Dimensions
  , noiseSpeed :: Number
  , particleSpeed :: Number
  , fieldForce :: Int
  , fieldSize :: Int
  , randomForce :: Int
  , cc :: ColorConfig
  }

type Particle = Record ParticleT


type PState = { noiseZ :: Number
              , hueCounter :: Number
              , particles :: Array Particle
              , field :: Array (Array Vec)
              }

newtype Radians = Radians Number
derive instance newtypeRadians :: Newtype Radians _

--------------------------------------------------------------------------------
-- VECTOR OPS

foreign import getLengthImpl :: forall r. Fn1 (STRecord r VecT) (ST r Number)
getLength :: forall r. STRecord r VecT -> ST r Number
getLength = runFn1 getLengthImpl

foreign import setLengthImpl :: forall r. Fn2 (STRecord r VecT) Number (ST r Unit)
setLength :: forall r. STRecord r VecT -> Number -> ST r Unit
setLength = runFn2 setLengthImpl


foreign import setAngleImpl :: forall r. Fn2 (STRecord r VecT) Radians (ST r Unit)
setAngle :: forall r. STRecord r VecT -> Radians -> ST r Unit
setAngle = runFn2 setAngleImpl

foreign import getAngleImpl :: forall r. Fn1 (STRecord r VecT) (ST r Radians)
getAngle :: forall r. STRecord r VecT -> ST r Radians
getAngle = runFn1 getAngleImpl

--------------------------------------------------------------------------------
-- PARTICLE OPS

foreign import moveImpl :: forall r. Fn3 (STRecord r ParticleT) Acceleration Number (ST r Unit)
move :: forall r. STRecord r ParticleT -> Acceleration -> Number -> ST r Unit
move = runFn3 moveImpl

foreign import wrapImpl :: forall r. Fn2 (STRecord r ParticleT) Dimensions (ST r Unit)
wrap :: forall r. STRecord r ParticleT -> Dimensions -> ST r Unit
wrap = runFn2 wrapImpl

foreign import addToImpl :: forall r. Fn2 (STRecord r VecT) Vec (ST r Unit)
addTo :: forall r. STRecord r VecT -> Vec -> ST r Unit
addTo = runFn2 addToImpl

foreign import setImpl :: forall r. Fn2 (STRecord r VecT) Vec (ST r Unit)
set :: forall r. STRecord r VecT -> Vec -> ST r Unit
set = runFn2 setImpl

--------------------------------------------------------------------------------
-- CONSTRUCTORS
calcExtents :: Dimensions -> Int -> FieldDim
calcExtents d i =
  { cols: floor $ d.width / toNumber i
  , rows: floor $ d.height / toNumber i
  }

mkParticle :: Number -> Number -> Effect Particle
mkParticle xdim ydim = do
  let p = { x: xdim, y: ydim}
      a = { x:0.0, y:0.0}
  s <- mul 2.0 <$> random
  v <- ({ x: _, y: _}) <$> ((_ - 0.5) <$> random)
                       <*> ((_ - 0.5) <$> random)
  pure { pos: p, prevPos: p, vel: v, acc: a, size: s}



--------------------------------------------------------------------------------
-- STATE

foreign import getStateImpl :: Effect PState
getState :: Effect PState
getState = getStateImpl

foreign import setStateImpl :: Fn1 PState Unit
setState :: PState -> Unit
setState = runFn1 setStateImpl


--------------------------------------------------------------------------------
-- INITIALIZATION

foreign import initFieldImpl :: EffectFn1 FieldDim Unit
initField :: FieldDim -> Effect Unit
initField = runEffectFn1 initFieldImpl

foreign import initParticlesImpl :: EffectFn1 (Array Particle) Unit
initParticles :: Array Particle -> Effect Unit
initParticles = runEffectFn1 initParticlesImpl

initState :: Config -> Effect Unit
initState conf = do
  let dim = conf.fieldDim
      wdim = conf.canvasDim
      nParticles = wdim.height * wdim.width / 100.0
  initField dim
  initParticles =<< (for (0 .. floor nParticles) $ \_ -> mkP wdim)
  where
    randomN n = (_ * n) <$> random
    mkP dim = do
      x' <- randomN dim.width
      y' <- randomN dim.height
      mkParticle x' y'

--------------------------------------------------------------------------------
-- FEILD CALCULATION

foreign import calculateFieldImpl :: EffectFn2 FieldDim (Fn3 Int Int Vec Unit) Unit
calculateField :: FieldDim -> (Int -> Int -> Vec -> Unit) -> Effect Unit
calculateField dims f = runEffectFn2 calculateFieldImpl dims (mkFn3 f)


foreign import incrNoiseImpl :: EffectFn1 Number Unit
incrNoise :: Number -> Effect Unit
incrNoise = runEffectFn1 incrNoiseImpl

foreign import incrHueImpl :: EffectFn1 Number Unit
incrHue :: Number -> Effect Unit
incrHue = runEffectFn1 incrHueImpl


--------------------------------------------------------------------------------
-- DRAWING

foreign import drawBackgroundImpl :: EffectFn3 Context2D Dimensions String Unit
drawBackground :: Context2D -> Dimensions -> Color -> Effect Unit
drawBackground ctx dim color = runEffectFn3 drawBackgroundImpl ctx dim (cssStringRGBA color)

foreign import drawParticlesImpl :: EffectFn3 Int FieldDim (EffectFn2 Particle Acceleration Unit) Unit
drawParticles :: Int -> FieldDim -> (Particle -> Acceleration -> Effect Unit) -> Effect Unit
drawParticles sz dims = runEffectFn3 drawParticlesImpl sz dims <<< mkEffectFn2

--------------------------------------------------------------------------------
-- SET UP AND MAIN

setUp :: String -> (Dimensions -> Config) -> Effect (Tuple Context2D Config)
setUp el mkConf = do
  mCanv <- getCanvasElementById el
  (flip $ maybe (throw $ "Canvas Element " <> el <> " not found")) mCanv $ \canv -> do
    dim <- getCanvasDimensions canv
    ctx <- getContext2D canv
    let conf = mkConf dim
    initState conf
    pure $ Tuple ctx conf
