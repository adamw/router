module Data.Coords where

import Prelude

type Coords =
  { x :: Number
  , y :: Number
  }

origin2D :: Coords
origin2D = { x: 0.0, y: 0.0 }

distance :: Coords -> Coords -> Number
distance c1 c2 = Math.sqrt $ (Math.pow (c1.x-c2.x) 2.0) + (Math.pow (c1.y-c2.y) 2.0)

addY :: Number -> Coords -> Coords
addY delta { x, y } = { x: x, y: y+delta }

addX :: Number -> Coords -> Coords
addX delta { x, y } = { x: x+delta, y: y }
