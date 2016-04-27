module View.Editor 
  ( setup
  --, updateEditorView
  ) where

import Prelude
import RouteEditor
import Pixi

import View.Actions

import Data.Function
import Data.Map as M
import Data.Foldable
import Data.Tuple as T
import Data.Int

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
  ha <- runFn3 newCircle 0.0 0.0 15.0
  _  <- runFn2 setHitArea ha g
  _  <- runFn3 setPosition stopCoords.x stopCoords.y g
  _  <- runFn2 addToContainer g btns
  _  <-        onMouseDown ch (Click stopId) g
  return unit

drawButton btns acc (T.Tuple stopId stopCoords) = do
  _ <- acc -- TODO
  g <- runFn0 newGraphics
  _ <- runFn3 beginFill (toNumber 0x4679BD) 1.0 g
  _ <- runFn4 lineStyle 2.0 (toNumber 0x4679BD) 1.0 g
  _ <- runFn4 drawCircle 0.0 0.0 15.0 g
  _ <- runFn1 endFill g
  _ <- runFn3 setPosition stopCoords.x stopCoords.y g
  _ <- runFn2 addToContainer g btns
  return unit

{-
update :: forall t. EditorView -> Editor -> PixiEff t EditorView
update ev e = ???

renderRoutesMap :: forall t. RoutesMap -> Graphics -> PixiEff t Unit
renderRoutesMap rm g = return unit

-}
