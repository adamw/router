module Data.Pair where

import Data.Tuple
import Prelude

data Pair a = Pair a a

instance showPair :: Show a => Show (Pair a) where
  show (Pair a1 a2) = "Pair (" <> show a1 <> ") (" <> show a2 <> ")"

instance eqPair :: Eq a => Eq (Pair a) where
  eq (Pair a11 a12) (Pair a21 a22) =
    if a11 == a21
    then a12 == a22
    else if a11 == a22
         then a12 == a21
         else false
              
instance ordPair :: Ord a => Ord (Pair a) where
  compare (Pair a11 a12) (Pair a21 a22) = 
    let a1o = if a11 < a12 then Tuple a11 a12 else Tuple a12 a11
        a2o = if a21 < a22 then Tuple a21 a22 else Tuple a22 a21
    in  case compare (fst a1o) (fst a2o) of
        EQ    -> compare (snd a1o) (snd a2o)
        other -> other

instance functorPair :: Functor Pair where
  map f (Pair a1 a2) = Pair (f a1) (f a2)

first :: forall a. Pair a -> a
first (Pair a _) = a

second :: forall a. Pair a -> a
second (Pair _ a) = a

toTuple :: forall a. Pair a -> Tuple a a
toTuple (Pair a1 a2) = Tuple a1 a2
