module View.Editor 
  ( setup
  --, updateEditorView
  ) where

import Prelude
import Editor
import Pixi

import View.Actions

import Data.Function
import Data.Map as M
import Data.Foldable
import Data.Tuple as T
import Data.Int
import Data.Coords

import Signal.Channel

type EditorView =
  { map :: RoutesMap
  , btnsLayer :: Container
  , gfxLayer :: Graphics
  }

setup :: forall t. Channel Action -> RoutesMap -> PixiChEff t EditorView
setup ch rm = do
  gfx <- runFn0 newGraphics
  btns <- setupButtons ch rm
  return { map: rm, btnsLayer: btns, gfxLayer: gfx }
  
setupButtons :: forall t. Channel Action -> RoutesMap -> PixiChEff t Container
setupButtons ch rm = do
  btns <- runFn0 newContainer
  _ <- foldl (setupButton ch btns) (return unit) (M.toList rm.stopsCoords)
  _ <- foldl (drawButton btns) (return unit) (M.toList rm.stopsCoords)
  return btns

setupButton ch btns acc (T.Tuple stopId stopCoords) = do
  _ <- acc
  g  <- runFn0 newGraphics
  _  <- runFn2 setInteractive true g
  _  <- runFn2 setButtonMode true g
  ha <- runFn2 newCircle origin2D 15.0
  _  <- runFn2 setHitArea ha g
  _  <- runFn2 setPosition stopCoords g
  _  <- runFn2 addToContainer g btns
  _  <-        onMouseDown ch (Click stopId) g
  return unit

drawButton btns acc (T.Tuple stopId stopCoords) = do
  _ <- acc -- TODO
  g <- runFn0 newGraphics
  _ <- runFn3 beginFill (Color 0x4679BD) opaque g
  _ <- runFn4 lineStyle (Width 2.0) (Color 0x4679BD) opaque g
  _ <- runFn3 drawCircle origin2D 15.0 g
  _ <- runFn1 endFill g
  _ <- runFn2 setPosition stopCoords g
  _ <- runFn2 addToContainer g btns
  return unit

{-
update :: forall t. EditorView -> Editor -> PixiEff t EditorView
update ev e = ???

renderRoutesMap :: forall t. RoutesMap -> Graphics -> PixiEff t Unit
renderRoutesMap rm g = return unit

-}
