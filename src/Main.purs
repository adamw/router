module Main where

import Math (sqrt)
import Prelude
import Control.Monad.Eff.Console

import Data.Foldable

import Data.Array
import Data.Maybe

import Control.Plus (empty)

import RouteEditor

--

--

    
--
--
--
--

main = print (diagonal 3.0 5.0)

diagonal :: Number -> Number -> Number
diagonal w h = sqrt (w*w+h*h)

add :: Number -> Number -> Number -> Number
add x y z = x +
  y + z

-- type Entry =
--   { first :: String
--   , last  :: String
--   }

-- showEntry :: Entry -> String
-- showEntry e = e.first ++ ", " ++ e.last

-- type AB = List Entry

-- findEntry :: String -> AB -> Maybe Entry
-- findEntry first = (head <<< filter filterEntry)
--   where
--   filterEntry e = e.first == first

-- fact :: Int -> Int
-- fact 0 = 1
-- fact n = n * fact (n-1)
-- z = Prelude.

import Control.MonadPlus (guard)

factors :: Int -> Array (Array Int)
factors n = do
  i <- 1 .. n
  j <- i .. n
  guard $ i * j == n
  return [i, j]

facsGt :: Int -> Int -> Array (Array Int)
facsGt n f =
  if n > 1
  then
    do
      guard $ f <= n
      i <- f .. n
      guard $ n `mod` i == 0
      let m = n / i
      let rest = facsGt m i
      (\fac -> i : fac) <$> rest
  else
    [[]]
  

facs :: Int -> Array (Array Int)
facs n = facsGt n 2

reverse :: Array Int -> Array Int
reverse a =
  foldl f [] a
  where
    f acc el = el : acc

data Shape
  = Circle Point Number
  | Rectangle Point Number Number
  | Line Point Point
  | Text Point String

data Point = Point
  { x :: Number
  , y :: Number
  }

newtype Pixels = Pixels Number
newtype Inches = Inches Number

p1 = Point { x: 10.0, y: 20.0 }

instance showPoint :: Show Point where
  show (Point { x, y }) = "{" ++ show x
                          ++ " " ++ show y ++ "}"

newtype Complex = Complex
  { real :: Number
  , img  :: Number
  }

instance showComplex :: Show Complex where
  show (Complex { real, img }) = show real ++ "i" ++ show img

instance eqComplex :: Eq Complex where
  eq (Complex { real = r1, img = i1 }) (Complex { real = r2, img = i2}) = r1 == r2 && i1 == i2

data Nonempty a = Nonempty a (Array a)

instance nonemptySemigroup :: (Semigroup (Array a)) => Semigroup (Nonempty a) where
  append (Nonempty h1 t1) (Nonempty h2 t2) = Nonempty h1 (append t1 (h2 : t2))

instance nonemptyShow :: (Show a, Show (Array a)) => Show (Nonempty a) where
  show (Nonempty h t) = (show h) ++ ", " ++ (show t)

instance nonemptyFunctor :: (Functor Array) => Functor Nonempty where
  map f (Nonempty h t) = Nonempty (f h) (map f t)

instance nonemptyFoldable :: (Foldable Array) => Foldable Nonempty where
  foldr f acc (Nonempty h t) = foldr f acc (h : t)
  foldl f acc (Nonempty h t) = foldl f acc (h : t)
  foldMap f (Nonempty h t) = foldMap f (h : t)

ne1 = Nonempty 1 [ 2, 3, 4 ]
ne2 = Nonempty 10 [ 20, 30, 40 ]

t1 ne = (+) 1 <$> ne
ne3 = t1 ne1

import Data.Monoid

class (Monoid m) <= Action m a where
  act :: m -> a -> a

-- act mempty a = a
-- act (x <> y) a = act x (act y a)

-- act (x <> (y <> z)) a = act x (act (y <> z) a) 
--                       = act x (act y (act z a))
--                       = act (x <> y) (act z a)
--                       = act ((x <> y) <> z) a

instance actionArray :: (Action m a) => Action m (Array a) where
  act m aa = (\a -> act m a)  <$> aa

newtype Self m = Self m

-- instance actionSelf :: Action m (Self m) where
--  act m (Self m2) = Self (m <> m2)

import Control.Apply

maybePlus m1 m2 = lift2 (+) m1 m2

person :: String -> (Array Int) -> String
person n a = "name: " ++ n ++ ", age: " ++ (show a)

import Data.Either
import Data.Validation

type Val a = V (Array String) a

n1 = "name1"
n2 = ""
n3 = "NAMe1"
a1 = [ 20, 30 ]
a2 = []

nonEmpty :: forall a. (Eq a, Monoid a) => String -> a -> Val a
nonEmpty err v =
  if v == mempty
  then invalid [ err ]
  else pure v 

import Data.String.Regex

matches :: String -> String -> Regex -> Val String
matches _ v r | test r v = pure v
matches err _ _ = invalid [ err ]

r1 :: String -> (Array Int) -> Val String
r1 n a = person <$> (nonEmpty "Name cannot be empty" n *>
                     matches "Name must be lowercase" n (regex "^[a-z0-9]+$" noFlags))
                <*> (nonEmpty "Ages cannot be empty" a)

nn1 = [ "name1", "name2", "name2" ]
nn2 = [ "name1", "Name1", "", "name3" ]

import Data.Traversable

valall :: forall t a. (Traversable t) => (a -> Val a) -> t a -> Val (t a)
valall = traverse

data Tree a = Leaf | Branch (Tree a) a (Tree a)

instance treeFunctor :: Functor Tree where
  map _ Leaf = Leaf
  map f (Branch l v r) = Branch (map f l) (f v) (map f r)

instance treeFoldable :: Foldable Tree where
  foldr _ a Leaf = a
  foldr f a (Branch l v r) = let
    s1 = foldr f a r
    s2 = f v s1
    in foldr f s2 l
  foldl _ a Leaf = a
  foldl f a (Branch l v r) = let
    s1 = foldl f a r
    s2 = f s1 v
    in foldl f s2 r
  foldMap = foldMapDefaultR

instance treeTraversable :: Traversable Tree where
  traverse _ Leaf = pure Leaf
  traverse f (Branch l v r) = let
    s1 = traverse f l
    s2 = f v
    s3 = traverse f r
    in Branch <$> s1 <*> s2 <*> s3
  sequence = sequenceDefault

instance showTree :: (Show a) => Show (Tree a) where
  show Leaf = "L"
  show (Branch l v r) = "[" ++ (show l) ++ "] " ++ (show v) ++ " [" ++ (show r) ++ "]"

valGt :: Int -> Int -> Val Int
valGt x y = if y >= x then pure y else invalid [ (show y) ++ " isn't greater than " ++ (show x) ]

tree1 = Branch (Branch Leaf 10 Leaf) 12 (Branch (Branch Leaf 15 Leaf) 20 Leaf)
