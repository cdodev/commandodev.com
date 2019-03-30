module Field where

import Prelude

import Color (hsla, cssStringRGBA)
import Control.Monad.ST (ST)
import Control.Monad.ST as ST
import Control.Monad.ST.Ref (STRef)
import Control.Monad.ST.Ref as ST
import Data.Array ((..), (!!), index)
import Data.Function.Uncurried (Fn1, runFn1)
import Data.Functor (void)
import Data.Int (toNumber, floor)
import Data.Lens (over, to, (^.))
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Symbol (SProxy(SProxy))
import Data.Traversable (for, sequence)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (logShow)
import Effect.Random (random)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Math as M
import Noise (seed, simplex3)
import P5 (P5, draw, getP5, setup)
import P5.Color (background3, fill, stroke)
import P5.Environment (frameRate2)
import P5.Rendering (createCanvas)
import P5.Shape (ellipse, rect, strokeWeight)
import Record.ST (STRecord)
import Record.ST as RecST

-- import P5.Structure
-- import P5.Transform
-- import P5.Typography

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
getLength :: forall r. STRecord r VecT -> ST r Number
getLength v = M.sqrt <$> (add <$> x2 <*> y2)
  where
    x2 = M.pow <$> RecST.peek x v <*> pure 2.0
    y2 = M.pow <$> RecST.peek y v <*> pure 2.0

setLength :: forall r. Number -> STRecord r VecT -> ST r Unit
setLength l v = do
  a <- getAngle v
  RecST.poke x (l * M.cos a) v
  RecST.poke y (l * M.sin a) v

setAngle :: forall r. Radians -> STRecord r VecT -> ST r Unit
setAngle (Radians n) v = do
  l <- getLength v
  RecST.poke x (l * M.cos n) v
  RecST.poke x (l * M.sin n) v

getAngle :: forall r. STRecord r VecT -> ST r Number
getAngle v = M.atan2 <$> RecST.peek y v <*> RecST.peek x v

--------------------------------------------------------------------------------
type Dimensions = { cols :: Int, rows :: Int }

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
type ParticleT = ( pos :: XY, prevPos :: XY, vel :: Velocity, acc :: Acceleration, size :: Number )

type Particle = Record ParticleT

mkParticle :: Number -> Number -> Effect Particle
mkParticle xdim ydim = do
  let p = { x: xdim, y: ydim}
      a = { x:0.0, y:0.0}
  s <- mul 4.0 <$> random
  v <- ({ x: _, y: _}) <$> ((_ - 0.5) <$> random)
                       <*> ((_ - 0.5) <$> random)
  pure { pos: p, prevPos: p, vel: v, acc: a, size: s}

move :: forall r. Acceleration -> STRecord r ParticleT -> ST r Unit
move accel recP = do
  v <- RecST.peek vel recP
  p <- RecST.peek pos recP
  a <- RecST.peek acc recP
  let newAcc = accel + a
      newVel = applyAcc newAcc v
      newPos = newVel + p
  RecST.poke prevPos p recP
  RecST.poke pos newPos recP
  RecST.poke vel newVel recP
  RecST.poke acc newAcc recP

wrapN :: Int -> Number -> Number
wrapN upper p
  | p > toNumber upper = 1.0
  | p < 1.0 = toNumber (upper - 1)
  | otherwise = p

wrap :: forall r. Dimensions -> STRecord r ParticleT -> ST r Unit
wrap d = RecST.modify pos f
  where
    f xy = { x: (wrapN d.cols xy.x), y:(wrapN d.rows xy.y) }

applyVel :: Velocity -> XY -> XY
applyVel = add

applyAcc :: Acceleration -> Velocity -> Velocity
applyAcc = add

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
  , windowDim :: Dimensions
  , noiseSpeed :: Number
  , particleSpeed :: Int
  , fieldForce :: Int
  , fieldSize :: Int
  , randomForce :: Int
  , cc :: ColorConfig
  }

mkConfig :: Dimensions -> Config
mkConfig dim =
  { zoom: 80
  , windowDim: dim
  , noiseSpeed: 0.01
  , particleSpeed: 5
  , fieldForce: 90
  , fieldSize: 3
  , randomForce: 10
  , cc: colorConf
  }

calcExtents :: Dimensions -> Int -> Dimensions
calcExtents d i = { cols: floor $ toNumber d.cols / toNumber i
                  , rows: floor $ toNumber d.rows / toNumber i}

--------------------------------------------------------------------------------

type Field r = Array (Array (STRecord r VecT)) -- Map (Tuple Int Int) Acceleration

--------------------------------------------------------------------------------
type PStateT r = ( noiseZ :: STRef r Number
                , hueCounter :: STRef r Number
                , particles :: Array (STRecord r ParticleT)
                , field :: Field r
                )

type PState r = Record (PStateT r)

foreign import setStateImpl :: forall r. Fn1 (PState r) Unit

foreign import getStateImpl :: forall r. ST r (PState r)

getState :: forall r. ST r (PState r)
getState = getStateImpl

setState :: forall r. PState r -> Unit
setState = runFn1 setStateImpl


initPST = { noiseZ: 0.0
          , hueCounter: 0.0
          }

--------------------------------------------------------------------------------
foreign import frameCountImpl :: EffectFn1 P5 Int

frameCount :: P5 -> Effect Int
frameCount = runEffectFn1 frameCountImpl

--------------------------------------------------------------------------------
type AppState = {
    p5 :: P5
  }

initialState :: Maybe AppState
initialState = Nothing

initAppState :: P5 -> Effect AppState
initAppState p5 = do
  pure { p5: p5 }


--------------------------------------------------------------------------------
initField :: forall r. Config -> ST r (Field r)
initField conf = do
  let dims = calcExtents conf.windowDim conf.fieldSize
  for (0..dims.rows) $ \_ ->
    for (0..dims.cols) $ \_ -> do
      RecST.thaw { x: 0.0, y: 0.0 }

calcField :: forall r. Config -> PState r -> ST r Unit
calcField cnf st = do
  let dims = calcExtents cnf.windowDim cnf.fieldSize
      z = toNumber cnf.zoom
      v = { x: 0.0, y: 0.0 }
      f = st.field
  noise <- ST.read st.noiseZ
  void $ for (0..dims.rows) $ \col ->
    for (0..dims.cols) $ \row -> do
      let x' = toNumber col
          y' = toNumber row
          dx = x'-(toNumber dims.cols)/2.0
          dy = y'-(toNumber dims.rows)/2.0
          a  = Radians $ (M.atan2 dy dx) + M.pi/2.0
          l = (M.sqrt (dx*dx + dy*dy))/100.0
          x1 = (_ / 2.0) $ simplex3 (x'/z) (y'/z) noise
          y1 = (_ / 2.0) $ simplex3 (x'/z + 40000.0) (y'/z + 40000.0) noise

      for (getP f row col) $ \accV -> do
        setAngle a accV
        setLength l accV
        RecST.modify x (_ + x1) accV
        RecST.modify y (_ + y1) accV

-- getP :: forall a. Array (Array a) -> Int -> Int -> Maybe a
getP f col row = join $ (_ !! col) <$> f !! row

stepParticles :: forall r. Config -> PState r -> ST r Unit
stepParticles conf ps = do
  let field = ps.field
  void $ for ps.particles $ \p -> do
    pPos <- RecST.peek pos p
    let x' = floor $ pPos.x / toNumber conf.fieldSize
        y' = floor $ pPos.y / toNumber conf.fieldSize
    for (getP field x' y') $ \v -> do
      flip move p =<< RecST.freeze v
      wrap conf.windowDim p


drawParticles :: forall r. P5 -> Config -> PState r -> ST r (Effect Unit)
drawParticles p5 conf ps = do
  hueC <- ST.read ps.hueCounter
  let h = (M.sin hueC * (toNumber conf.cc.hueRange)) + toNumber conf.cc.baseHue
      c = hsla h (toNumber conf.cc.sat) 0.5 conf.cc.opacity
      drawP pos size = do
        fill p5 $ cssStringRGBA c
        ellipse p5 pos.x pos.y size Nothing
  map (void <<< sequence) $ for ps.particles $ \p -> do
    drawP <$> RecST.peek pos p <*> RecST.peek size p


initState :: forall r. Config -> Array Particle -> ST r (PState r)
initState conf particles = do
  field <- initField conf
  ps <- for particles $ RecST.thaw
  n <- ST.new 0.0
  h <- ST.new 0.0
  pure { noiseZ: n, hueCounter: h, field: field, particles: ps }

main :: Maybe AppState -> Effect (Maybe AppState)
main mAppState = do
  let w = 800
      h = 800
      dim = { cols: w, rows: h}
      conf = mkConfig dim
      nParticles = w * h / 100
  p <- maybe getP5 (\st -> pure st.p5) mAppState
  _ <- seed =<< random
  ps <- for (0 .. nParticles) $ \_ -> mkP dim

  let palette =
        { a: "#4d0c40"
        , b: "#a11a23"
        , c: "#b29179"
        , d: "#c0a476"
        , e: "#9d7f38"
        }
  let _ = ST.run (do
           s <- initState conf ps
           pure $ setState s )
  setup p do
    frameRate2 p 30.0
    _ <- createCanvas p (toNumber w) (toNumber h) Nothing
    -- n <- random
    pure unit

  draw p do
    background3 p "black" Nothing
    let _ = (ST.run do
                state <- getState
             --
                calcField conf state
                stepParticles conf state
                pure $ setState state)
    ST.run (drawParticles p conf =<< getState)
    let _ = ST.run (do
              state <- getState
              void $ ST.modify (_ + conf.noiseSpeed) state.noiseZ
              void $ ST.modify (_ + conf.cc.hueSpeed) state.hueCounter)
    pure unit

  pure $ Just { p5: p }

  where
    randomN n = (*) (toNumber n) <$> random
    mkP dim = do
      x <- randomN dim.cols
      y <- randomN dim.rows
      mkParticle x y
