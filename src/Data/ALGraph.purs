module Data.ALGraph -- adjacency list graph
  ( ALGraph()
  , empty
  ) where

import Prelude

import Data.Map as M
import Data.Tuple as T
import Data.List as L
import Data.Maybe

newtype ALGraph v e = ALGraph (M.Map v (L.List (T.Tuple e v)))

empty :: forall v e. Ord v => ALGraph v e
empty = ALGraph M.empty

addV :: forall v e. Ord v => v -> ALGraph v e -> ALGraph v e
addV v (ALGraph m) = ALGraph (M.insert v L.Nil m)

addDirectedE :: forall v e. Ord v => v -> v -> e -> ALGraph v e -> ALGraph v e
addDirectedE v1 v2 e alg@(ALGraph m) = fromMaybe alg $ do
  n1 <- M.lookup v1 m
  n2 <- M.lookup v2 m
  let n1' = L.Cons (T.Tuple e v2) n1
  return $ ALGraph (M.insert v1 n1' m)

addE :: forall v e. Ord v => v -> v -> e -> ALGraph v e -> ALGraph v e
addE v1 v2 e a = addDirectedE v1 v2 e <<< addDirectedE v2 v1 e $ a

vertices :: forall v e. ALGraph v e -> L.List v
vertices (ALGraph m) = M.keys m

edgesFrom :: forall v e. Ord v => v -> ALGraph v e -> L.List (T.Tuple e v)
edgesFrom v (ALGraph m) = fromMaybe L.Nil $ M.lookup v m
