module Field where

import Prelude

import Color (hsla, cssStringRGBA)
import Control.MonadPlus (empty)
import Control.MonadZero (guard)
import Data.Array ((..))
import Data.Int (toNumber, floor)
import Data.Lens (over, (^.), to, Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Symbol (SProxy(SProxy))
import Data.Traversable (for, for_)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (logShow)
import Effect.Random (random)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Math as M
import P5 (P5, draw, getP5, setup)
import P5.Color (background3, fill, stroke)
import Noise (seed, simplex3)
import P5.Rendering (createCanvas)
import P5.Shape (ellipse, rect, strokeWeight)
-- import P5.Structure
-- import P5.Transform
-- import P5.Typography

type Vec = { x :: Number, y :: Number }

newtype Radians = Radians Number
derive instance newtypeRadians :: Newtype Radians _

--------------------------------------------------------------------------------
getLength :: Vec -> Number
getLength v = M.sqrt (x2 + y2)
  where
    x2 = M.pow v.x 2.0
    y2 = M.pow v.y 2.0

setLength :: Number -> Vec -> Vec
setLength l v =
  let a = getAngle v
  in { x: l * M.cos a, y: l * M.sin a}

setAngle :: Radians -> Vec -> Vec
setAngle (Radians n) v =
  let l = getLength v
  in { x: l * M.cos n, y: l * M.sin n}

getAngle :: Vec -> Number
getAngle v = M.atan2 v.y v.x

dist :: Vec -> Vec -> Number
dist a b = getLength $ a - b

--------------------------------------------------------------------------------
newtype Dimensions = Dimensions { x :: Int, y :: Int }
derive instance newtypeDimensions :: Newtype Dimensions _
instance showDimensions :: Show Dimensions where
  show (Dimensions p) = "Dimensions " <> show p

--------------------------------------------------------------------------------
newtype Acceleration = Acceleration Vec
derive instance newtypeAccelleration :: Newtype Acceleration _
instance showAcceleration :: Show Acceleration where
  show (Acceleration p) = "Acceleration " <> show p

--------------------------------------------------------------------------------
newtype Velocity = Velocity Vec
derive instance newtypeVelocity :: Newtype Velocity _
instance showVelocity :: Show Velocity where
  show (Velocity p) = "Velocity " <> show p


--------------------------------------------------------------------------------
newtype XY = XY Vec
derive instance newtypeXY :: Newtype XY _
instance showXY :: Show XY where
  show (XY p) = "XY " <> show p

--------------------------------------------------------------------------------
newtype Particle = Particle { pos :: XY, vel :: Velocity, acc :: Acceleration }
derive instance newtypeParticle :: Newtype Particle _
instance showParticle :: Show Particle where
  show (Particle p) = "Particle " <> show p

mkParticle :: Number -> Number -> Particle
mkParticle x y =
  let p = XY { x: x, y: y}
      v = Velocity { x:0.0, y:0.0}
      a = Acceleration { x:0.0, y:0.0}

  in Particle { pos: p, vel: v, acc: a}
move :: Acceleration -> Particle -> Particle
move acc (Particle p ) =
  let newVel = applyAcc acc p.vel
      newPos = applyVel newVel p.pos
  in Particle { pos: newPos, vel: newVel, acc: acc }

wrapN :: Int -> Number -> Number
wrapN upper p
  | p > toNumber upper = 0.0
  | p < 0.0 = toNumber (upper - 1)
  | otherwise = p

wrap :: Dimensions -> Particle -> Particle
wrap (Dimensions d) = over (_Newtype <<< prop (SProxy :: SProxy "pos") <<< _Newtype) f
  where
    f xy = { x: (wrapN d.x xy.x), y:(wrapN d.y xy.y) }

applyVel :: Velocity -> XY -> XY
applyVel = applyNTPos

applyAcc :: Acceleration -> Velocity -> Velocity
applyAcc = applyNTPos

applyNTPos :: forall a b . Newtype a Vec => Newtype b Vec => a -> b -> b
applyNTPos nt = over _Newtype (addPos $ unwrap nt)

addPos :: Vec -> Vec -> Vec
addPos a b = { x: a.x + b.x, y: a.y + b.y }


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

mkConfig :: Int -> Int -> Config
mkConfig w h =
  { zoom: 80
  , windowDim: Dimensions { x: w, y: h }
  , noiseSpeed: 0.01
  , particleSpeed: 2
  , fieldForce: 90
  , fieldSize: 3
  , randomForce: 10
  , cc: colorConf
  }

--------------------------------------------------------------------------------

type Field = Map (Tuple Int Int) Acceleration

--------------------------------------------------------------------------------
type PState = { noiseZ :: Number
              , hueCounter :: Number
              , particles :: Array Particle
              }


foreign import setStateImpl :: EffectFn1 PState Void

foreign import getStateImpl :: Effect PState

getState :: Effect PState
getState = getStateImpl

setState :: PState -> Effect Void
setState = runEffectFn1 setStateImpl


initState :: PState
initState = { noiseZ: 0.0
            , hueCounter: 0.0
            , particles: []
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

initAppState :: Dimensions -> P5 -> Effect AppState
initAppState (Dimensions dim) p5 = do
  pure { p5: p5 }

calcField :: P5 -> Config -> PState -> Effect Field
calcField p5 cnf st = do
  let cols = cnf.windowDim ^. to unwrap <<< prop (SProxy :: SProxy "x") / cnf.fieldSize
      rows = cnf.windowDim ^. to unwrap <<< prop (SProxy :: SProxy "y") / cnf.fieldSize
      keys = Tuple <$> (0 .. cols)
                   <*> (0 .. rows)
      z = toNumber cnf.zoom
      v = { x: 0.0, y: 0.0 }
  kvs <- for keys $ \kv@(Tuple x y) -> do
    let x' = toNumber x
        y' = toNumber y
        dx = x'-(toNumber cols)/2.0
        dy = y'-(toNumber rows)/2.0
        a  = Radians $ (M.atan2 dy dx) + M.pi/2.0
        l = (M.sqrt (dx*dx + dy*dy))/100.0
        v' = setLength l $ setAngle a v
    x1 <- (_ / 2.0) <$> simplex3 (x'/z) (y'/z) (st.noiseZ)
    y1 <- (_ / 2.0) <$> simplex3 (x'/z + 4000.0) (y'/z + 4000.0) (st.noiseZ)
    pure $ Tuple kv $ Acceleration $ v' + { x: x1, y:y1 }
  pure $ Map.fromFoldable kvs

drawParticles :: P5 -> Config -> PState -> Field -> Effect (Array Particle)
drawParticles p5 conf ps field = do
  let h = (M.sin ps.hueCounter * (toNumber conf.cc.hueRange)) + toNumber conf.cc.baseHue
      c = hsla h (toNumber conf.cc.sat) 0.5 conf.cc.opacity
  fill p5 $ cssStringRGBA c
  for ps.particles $ \p -> do
    let x = floor $ posXY p (_.x) / toNumber conf.fieldSize
        y = floor $ posXY p (_.y) / toNumber conf.fieldSize
    let mv = Map.lookup (Tuple x y) field
    mp <- for mv $ \v -> do
      drawP <<< wrap conf.windowDim $ move v p
    pure $ fromMaybe p mp
  where
    posXY :: Particle -> (Vec -> Number) -> Number
    posXY (Particle p) f = f $ unwrap p.pos
    drawP p = do
      -- sz <- random
      ellipse p5 (posXY p (_.x)) (posXY p (_.y)) 4.0 Nothing
      pure p

main :: Maybe AppState -> Effect (Maybe AppState)
main mAppState = do
  let w = 800
      h = 400
      dim = Dimensions { x: w, y: h}
      conf = mkConfig w h
  p <- maybe getP5 (\x -> pure x.p5) mAppState

  let palette =
        { a: "#4d0c40"
        , b: "#a11a23"
        , c: "#b29179"
        , d: "#c0a476"
        , e: "#9d7f38"
        }
  setup p do
    _ <- createCanvas p (toNumber w) (toNumber h) Nothing
    -- n <- random
    _ <- seed (10000.0)
    let nParticles = w * h / 200
    ps <- for (0 .. nParticles) $ \_ -> mkP dim
    _ <- setState (initState { particles = ps })
    pure unit

  draw p do
    s <- getState
    -- logShow s.noiseZ
    field <- calcField p conf s
    background3 p "black" Nothing
    p' <- drawParticles p conf s field
    _ <- setState (s { noiseZ = s.noiseZ + conf.noiseSpeed
                     , hueCounter = s.hueCounter + conf.cc.hueSpeed
                     , particles = p'})
    pure unit

  pure $ Just { p5: p }

  where
    randomN n = (*) (toNumber n) <$> random
    mkP (Dimensions dim) = mkParticle <$> randomN dim.x <*> randomN dim.y
