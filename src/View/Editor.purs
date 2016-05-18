module View.Editor 
  ( setup
  , draw
  , EditorView
  --, updateEditorView
  ) where

import Prelude
import Editor
import Pixi

import View.Actions

import Data.Function
import Data.Map as M
import Data.Set as S
import Data.Foldable
import Data.Tuple as T
import Data.Coords

import Signal.Channel

type EditorView =
  { btnsLayer :: Container
  , gfxLayer :: Graphics
  }

setup :: forall t. Channel Action -> RoutesMap -> PixiChEff t EditorView
setup ch rm = do
  gfx  <- runFn0 newGraphics
  _    <- draw gfx rm
  btns <- setupButtons ch rm
  return { btnsLayer: btns, gfxLayer: gfx }
  
setupButtons :: forall t. Channel Action -> RoutesMap -> PixiChEff t Container
setupButtons ch rm = do
  btns <- runFn0 newContainer
  _    <- foldl (setupButton ch btns) (return unit) (M.toList rm.stopsCoords)
  return btns

setupButton ch btns acc (T.Tuple stopId stopCoords) = acc >>= \_ -> do
  g  <- runFn0 newGraphics
  _  <- runFn2 setInteractive true g
  _  <- runFn2 setButtonMode true g
  ha <- runFn2 newCircle origin2D 15.0
  _  <- runFn2 setHitArea ha g
  _  <- runFn2 setPosition stopCoords g
  _  <- runFn2 addToContainer g btns
  _  <-        onMouseDown ch (Click stopId) g
  return unit

draw :: forall t. Graphics -> RoutesMap -> PixiEff t Unit
draw gfx rm = do
  _ <- runFn1 clear gfx
  _ <-        foldl (drawButton gfx rm) (return unit) (M.toList rm.stopsCoords)
  return unit
    
drawButton btns rm acc (T.Tuple stopId stopCoords) = let
  inside g = do
    _ <- runFn3 beginFill (Color 0x4679BD) opaque g
    _ <- runFn4 lineStyle (Width 2.0) (Color 0x4679BD) opaque g
    _ <- runFn3 drawCircle origin2D 15.0 g
    _ <- runFn1 endFill g
    return unit
  outsideIfSelected g = if S.member stopId rm.selected
    then do
      _ <- runFn4 lineStyle (Width 5.0) (Color 0xcfdc00) opaque g
      _ <- runFn3 drawCircle origin2D 19.0 g
      return unit
    else return unit
  in acc >>= \_ -> do
    g <- runFn0 newGraphics
    _ <- inside g
    _ <- outsideIfSelected g
    _ <- runFn2 setPosition stopCoords g
    _ <- runFn2 addToContainer g btns
    return unit

{-
update :: forall t. EditorView -> Editor -> PixiEff t EditorView
update ev e = ???

renderRoutesMap :: forall t. RoutesMap -> Graphics -> PixiEff t Unit
renderRoutesMap rm g = return unit

-}
