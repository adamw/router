module Main where

import Math (sqrt, pow)
import Prelude

import Data.Foldable

import Data.Array
import Data.Maybe
import Data.List as L
import Data.Int (toNumber, floor)
import Data.Function

import Control.Plus (empty)

import Control.Monad.Eff
import Control.Monad.Eff.Random
import Control.Monad.Eff.Console
import Control.Monad.ST

import Control.Monad.Eff.Console

import Signal
import Signal.DOM as SignalDOM

import RouteEditor
import Pixi
import Fps
import TheCity

type ViewState =
  { renderer :: Renderer
  , stage :: Container
  , fps :: Fps
  , editor :: Editor
  }

main = do
  state <- setup
  animationSignal <- SignalDOM.animationFrame
  let stepSignal = foldp step state animationSignal
  let renderSignal = render <$> stepSignal
  runSignal renderSignal

setup :: forall r. PixiEff r ViewState
setup = do
  r <- runFn2 newRenderer 640 480
  _ <- runFn2 setBackgroundColor 0x555555 r
  _ <- appendRendererToBody r
  s <- runFn0 newContainer
  fps <- setupFps s
  let editor = emptyEditor theCity
  return { renderer: r, stage: s, fps: fps, editor: editor }

step :: Number -> ViewState -> ViewState
step nowMillis state = let
  fps' = updateFps (floor (nowMillis / 1000.0)) state.fps
  in state { fps = fps' }

render :: forall r. ViewState -> PixiEff r Unit
render state = do
  _ <- renderFps state.fps
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
