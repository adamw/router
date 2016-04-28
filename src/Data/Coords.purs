module Data.Coords where

type Coords =
  { x :: Number
  , y :: Number
  }

origin2D :: Coords
origin2D = { x: 0.0, y: 0.0 }
