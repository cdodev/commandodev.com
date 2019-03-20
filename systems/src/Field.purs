module Field where

import Prelude
import Data.Maybe (Maybe(..), maybe)
import Data.Lens
import Data.Int (toNumber)
import Math as M
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Symbol (SProxy(SProxy))
import Data.Lens.Record (prop)
import Data.Newtype hiding (over)

import Effect (Effect)
import Effect.Console (log)
import P5.Types (Vector)
import P5
import P5.Color
import P5.Data
import P5.Environment
import P5.Events
import P5.IO
import P5.Image
import P5.LightsAndCamera
import P5.Math
import P5.Rendering
import P5.Shape
import P5.Structure
import P5.Transform
import P5.Typography

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
type AppState = {
  p5 :: P5
  }

initialState :: Maybe AppState
initialState = Nothing

main :: Maybe AppState -> Effect (Maybe AppState)
main mAppState = do
  let w = 600.0
      h = 600.0
  p <- maybe getP5 (\x -> pure x.p5) mAppState

  let palette =
        { a: "#4d0c40"
        , b: "#a11a23"
        , c: "#b29179"
        , d: "#c0a476"
        , e: "#9d7f38"
        }
  setup p do
    _ <- createCanvas p w h Nothing
    pure unit

  draw p do
    background3 p palette.b Nothing
    stroke p palette.a
    strokeWeight p 5.0
    rect p 100.0 100.0 50.0 50.0 Nothing Nothing
    rect p 110.0 110.0 50.0 50.0 Nothing Nothing
    pure unit

  pure $ Just { p5: p }
