module Data.UnorderedPair where

import Data.Tuple
import Prelude

data UnorderedPair a = UnorderedPair a a

instance showUnorderePair :: Show a => Show (UnorderedPair a) where
  show (UnorderedPair a1 a2) = "UnorderedPair (" ++ show a1 ++ ") (" ++ show a2 ++ ")"

instance eqUnorderedPair :: Eq a => Eq (UnorderedPair a) where
  eq (UnorderedPair a11 a12) (UnorderedPair a21 a22) =
    if a11 == a21
    then a12 == a22
    else if a11 == a22
         then a12 == a21
         else false
              
instance ordUnorderedPair :: Ord a => Ord (UnorderedPair a) where
  compare (UnorderedPair a11 a12) (UnorderedPair a21 a22) = 
    let a1o = if a11 < a12 then Tuple a11 a12 else Tuple a12 a11
        a2o = if a21 < a22 then Tuple a21 a22 else Tuple a22 a21
    in  case compare (fst a1o) (fst a2o) of
        EQ    -> compare (snd a1o) (snd a2o)
        other -> other

instance functorUnorderedPair :: Functor UnorderedPair where
  map f (UnorderedPair a1 a2) = UnorderedPair (f a1) (f a2)

first :: forall a. UnorderedPair a -> a
first (UnorderedPair a _) = a

second :: forall a. UnorderedPair a -> a
second (UnorderedPair _ a) = a

toTuple :: forall a. UnorderedPair a -> Tuple a a
toTuple (UnorderedPair a1 a2) = Tuple a1 a2
