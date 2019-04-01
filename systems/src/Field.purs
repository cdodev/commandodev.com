module Field where

import Prelude

import Data.Tuple (Tuple(..))
import Color (hsla, cssStringHSLA)
import Control.Monad.ST (ST)
import Control.Monad.ST as ST
import Data.Array ((!!), (..))
import Data.Function.Uncurried (Fn1, Fn2, Fn3, mkFn3, runFn1, runFn2, runFn3)
import Data.Int (toNumber, floor)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(SProxy))
import Data.Traversable (for)
import Effect (Effect)
import Effect.Exception (throw)
import Effect.Random (random)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, mkEffectFn2, runEffectFn1, runEffectFn2, runEffectFn3)
import Math as M
import Noise (seed, simplex3)
import Graphics.Canvas
import Record.ST (STRecord)
import Record.ST as RecST
import Unsafe.Coerce (unsafeCoerce)
import Web.HTML (window)
import Web.HTML.Window (requestAnimationFrame)


x = SProxy :: SProxy "x"
y = SProxy :: SProxy "y"

pos = SProxy :: SProxy "pos"
prevPos = SProxy :: SProxy "prevPos"
vel = SProxy :: SProxy "vel"
acc = SProxy :: SProxy "acc"
size = SProxy :: SProxy "size"


type VecT = ( x :: Number, y :: Number )

type Vec = Record VecT

type Acceleration = Vec
type Velocity = Vec
type XY = Vec


--------------------------------------------------------------------------------
newtype Radians = Radians Number
derive instance newtypeRadians :: Newtype Radians _

--------------------------------------------------------------------------------
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
type FieldDim = { cols :: Int, rows :: Int }

--------------------------------------------------------------------------------

foreign import calculateFieldImpl :: EffectFn2 FieldDim (Fn3 Int Int Vec Unit) Unit

calculateField :: FieldDim -> (Int -> Int -> Vec -> Unit) -> Effect Unit
calculateField dims f = runEffectFn2 calculateFieldImpl dims (mkFn3 f)

--------------------------------------------------------------------------------
type ParticleT = ( pos :: XY, prevPos :: XY, vel :: Velocity, acc :: Acceleration, size :: Number )

type Particle = Record ParticleT

mkParticle :: Number -> Number -> Effect Particle
mkParticle xdim ydim = do
  let p = { x: xdim, y: ydim}
      a = { x:0.0, y:0.0}
  s <- mul 2.0 <$> random
  v <- ({ x: _, y: _}) <$> ((_ - 0.5) <$> random)
                       <*> ((_ - 0.5) <$> random)
  pure { pos: p, prevPos: p, vel: v, acc: a, size: s}

foreign import moveImpl :: forall r. Fn3 (STRecord r ParticleT) Acceleration Number (ST r Unit)

move :: forall r. STRecord r ParticleT -> Acceleration -> Number -> ST r Unit
move = runFn3 moveImpl

foreign import wrapImpl :: forall r. Fn2 (STRecord r ParticleT) Dimensions (ST r Unit)

wrap :: forall r. STRecord r ParticleT -> Dimensions -> ST r Unit
wrap = runFn2 wrapImpl

foreign import addToImpl :: forall r. Fn2 (STRecord r VecT) Vec (ST r Unit)
addTo :: forall r. STRecord r VecT -> Vec -> ST r Unit
addTo = runFn2 addToImpl

foreign import drawBackgroundImpl :: EffectFn2 Context2D Dimensions Unit

drawBackground :: Context2D -> Dimensions -> Effect Unit
drawBackground = runEffectFn2 drawBackgroundImpl

--------------------------------------------------------------------------------
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
            , sat: 100
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

mkConfig :: Int -> Dimensions -> Config
mkConfig fSize dim =
  { zoom: 80
  , fieldDim: calcExtents dim fSize
  , canvasDim: dim
  , noiseSpeed: 0.01
  , particleSpeed: 5.0
  , fieldForce: 90
  , fieldSize: fSize
  , randomForce: 10
  , cc: colorConf
  }

calcExtents :: Dimensions -> Int -> FieldDim
calcExtents d i = { cols: floor $ d.width / toNumber i
                  , rows: floor $ d.height / toNumber i}

--------------------------------------------------------------------------------

type Field = Array (Array Vec) -- Map (Tuple Int Int) Acceleration

--------------------------------------------------------------------------------
type PState = { noiseZ :: Number
              , hueCounter :: Number
              , particles :: Array Particle
              , field :: Field
              }

foreign import setStateImpl :: Fn1 PState Unit

foreign import getStateImpl :: Effect PState

getState :: Effect PState
getState = getStateImpl

setState :: PState -> Unit
setState = runFn1 setStateImpl


--------------------------------------------------------------------------------
foreign import initFieldImpl :: EffectFn1 FieldDim Unit

initField :: FieldDim -> Effect Unit
initField = runEffectFn1 initFieldImpl

foreign import initParticlesImpl :: EffectFn1 (Array Particle) Unit

initParticles :: Array Particle -> Effect Unit
initParticles = runEffectFn1 initParticlesImpl

calcField :: Config -> PState -> Effect Unit
calcField cnf st = do
  let dims = cnf.fieldDim
      z = toNumber cnf.zoom
      toRec :: forall r. Acceleration -> STRecord r VecT
      toRec = unsafeCoerce
      fn :: forall r. Int -> Int -> Acceleration -> ST r Unit
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
        addTo accV { x: x1, y: y1 }
  calculateField dims (\c r v -> ST.run (fn c r v))
-- getP :: forall a. Array (Array a) -> Int -> Int -> Maybe a
getP f col row = join $ (_ !! col) <$> f !! row

foreign import incrNoiseImpl :: EffectFn1 Number Unit

incrNoise :: Number -> Effect Unit
incrNoise = runEffectFn1 incrNoiseImpl

foreign import incrHueImpl :: EffectFn1 Number Unit
incrHue :: Number -> Effect Unit
incrHue = runEffectFn1 incrHueImpl

foreign import drawParticlesImpl :: EffectFn3 Int FieldDim (EffectFn2 Particle Acceleration Unit) Unit

drawParticles :: Int -> FieldDim -> (Particle -> Acceleration -> Effect Unit) -> Effect Unit
drawParticles sz dims = runEffectFn3 drawParticlesImpl sz dims <<< mkEffectFn2

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

draw :: Context2D -> Config -> Effect Unit
draw ctx conf = do
  state <- getState
  drawBackground ctx conf.canvasDim
  void $ requestAnimationFrame (draw ctx conf) =<< window
  calcField conf state
  drawAll ctx conf state
  incrNoise conf.noiseSpeed
  incrHue conf.cc.hueSpeed

setUp :: String -> Effect (Tuple Context2D Config)
setUp el = do
  mCanv <- getCanvasElementById el
  (flip $ maybe (throw $ "Canvas Element " <> el <> " not found")) mCanv $ \canv -> do
    dim <- getCanvasDimensions canv
    ctx <- getContext2D canv
    let conf = mkConfig 5 dim
    initState conf
    pure $ Tuple ctx conf

main :: Effect Unit
main = do
  state <- getState
  _ <- seed =<< random

  (Tuple ctx conf) <- setUp "canvas"
  draw ctx conf
