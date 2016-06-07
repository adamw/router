module Main where

import Prelude
import Data.Foldable
import Data.Array
import Data.Maybe
import Data.Function
import Data.Coords
import Control.Apply
import Control.Monad.Eff
import Control.Monad.Eff.Random
import Control.Monad.Eff.Console
import Control.Monad.ST
import Control.Monad.Eff.Console
import Signal
import Editor
import Pixi
import TheCity
import View.Actions
import Data.List as L
import Signal.Channel as SignalCh
import Signal.DOM as SignalDOM
import View.Editor as EditorView
import View.EditorControl as EditorControlView
import View.Fps as FpsView
import View.Messages as MsgsView
import Control.Plus (empty)
import Data.Int (toNumber, floor)
import Math (sqrt, pow)
import City as City

type ViewState =
  { renderer :: Renderer
  , stage :: Container
  , fps :: FpsView.Fps
  , editor :: Editor
  , editorView :: EditorView.EditorView
  , editorControlView :: Container
  , msgs :: MsgsView.Msgs
  , updated :: Boolean
  }

main = do
  actionCh     <- SignalCh.channel NoOp
  initialState <- setup actionCh
  animationSig <- SignalDOM.animationFrame
  let mainSig   = merge (SignalCh.subscribe actionCh) (AnimationFrame <$> animationSig)
  let stepSig   = foldp step initialState mainSig
  let renderSig = render <$> stepSig
  runSignal renderSig

setup :: forall r. (SignalCh.Channel Action) -> PixiChEff r ViewState
setup ch = do
  r    <- runFn2 newRenderer 640 480
  _    <- runFn2 setBgColor 0x555555 r
  _    <-        appendRendererToBody r
  s    <- runFn0 newContainer
  fps  <- FpsView.setup s
  msgs <- MsgsView.setup s
  let editor = emptyEditor theCity
  editorView <- EditorView.setup ch editor.city (createMap editor)
  _    <- runFn2 addToContainer editorView.btnsLayer s
  _    <- runFn2 addToContainer editorView.gfxLayer  s
  editorControlView <- runFn0 newContainer
  _    <- runFn2 addToContainer editorControlView s
  _    <- runFn2 setPosition { x: City.width editor.city, y: 0.0 } editorControlView
  return { renderer: r
         , stage: s
         , fps: fps
         , editor: editor
         , editorView: editorView
         , editorControlView: editorControlView
         , msgs: msgs
         , updated: true }

step :: Action -> ViewState -> ViewState
step (AnimationFrame nowMillis) state = state
  { fps     = FpsView.update (floor (nowMillis / 1000.0)) state.fps
  , updated = false
  }
step (Click stopId) state = state
  { msgs    = MsgsView.update ("You clicked " ++ (show stopId)) state.msgs
  , editor  = selectStop stopId state.editor
  , updated = true
  }
step (Hover stopId) state = state
  { msgs    = MsgsView.update ("Hovering " ++ (show stopId)) state.msgs
  , editor  = candidateStop stopId state.editor
  , updated = true
  }
step NoOp state = state

render :: forall r. ViewState -> PixiChEff r Unit
render state = do
  _ <- FpsView.render state.fps
  _ <- MsgsView.render state.msgs
  _ <- if state.updated      
       then
         EditorView.draw state.editorView.gfxLayer state.editor.city (createMap state.editor) *>
         EditorControlView.draw state.editorControlView state.editor
       else return unit
  _ <- runFn2 renderContainer state.stage state.renderer
  return unit

--renderEditor :: forall r. Editor -> PixiEff r Unit
--renderEditor e = let
--  view = createView e

--main = do
--  pi <- estimatePi 1000
--  print pi

addUp :: forall eff h. Int -> Int -> Eff (st :: ST h | eff) Int
addUp a b = do
  r1 <- newSTRef a
  modifySTRef r1 (b + _)
  modifySTRef r1 (b + _)
  readSTRef r1

run = pureST (addUp 2 3)

inCircle :: Number -> Number -> Boolean
inCircle x y = (sqrt $ (pow (x - 0.5) 2.0) + (pow (y - 0.5) 2.0)) < 0.5

estimatePi :: forall eff h. Int -> Eff (st :: ST h, random :: RANDOM | eff) Number
estimatePi n = do
  c <- newSTRef 0
  forE 0.0 (toNumber n) $ \i -> do
    x <- random
    y <- random
    _ <- if (inCircle x y) then modifySTRef c (1 +) else return 0
    return unit
  f <- readSTRef c
  return $ 4.0 * (toNumber f) / (toNumber n)

diagonal :: Number -> Number -> Number
diagonal w h = sqrt (w*w+h*h)

add :: Number -> Number -> Number -> Number
add x y z = x +
  y + z

safeDivide :: Int -> Int -> Maybe Int
safeDivide _ 0 = Nothing
safeDivide x y = Just (x / y)

foldM :: forall m a b. (Monad m) => (a -> b -> m a) -> a -> L.List b -> m a
foldM _ a L.Nil = return a
foldM f a (L.Cons b bs) = do
  a' <- f a b
  foldM f a' bs

third :: forall a. Array a -> Maybe a
third a = do
  t1 <- tail a
  t2 <- tail t1
  head t2

sums a = nub $ sort $ foldM (\x -> \y -> [ x, y, x + y ]) 0 (L.toList a) 

filterM :: forall m a. (Monad m) => (a -> m Boolean) -> L.List a -> m (L.List a)
filterM _ L.Nil = return L.Nil
filterM p (L.Cons h t) = do
  include <- p h
  o <- filterM p t
  return if (include) then L.Cons h o else o

testX :: forall t. (Show t, Eq t) => t -> t
testX t = t
