module Data.ALGraph -- adjacency list graph
  ( ALGraph()
  , empty
  , addV
  , addVs
  , addDirectedE
  , addE
  , vertices
  , edgesFrom
  , shortestPath
  ) where

import Prelude

import Data.Map as M
import Data.Tuple as T
import Data.List as L
import Data.Sequence.NonEmpty as NE
import Data.Maybe
import Data.Sequence.Ordered as OrdSeq
import Data.Monoid
import Data.Foldable (class Foldable, foldl)

type Edge v e = T.Tuple e v
newtype ALGraph v e = ALGraph (M.Map v (L.List (Edge v e)))

empty :: forall v e. Ord v => ALGraph v e
empty = ALGraph M.empty

addV :: forall v e. Ord v => v -> ALGraph v e -> ALGraph v e
addV v (ALGraph m) = ALGraph (M.insert v L.Nil m)

addVs :: forall f v e. (Foldable f, Ord v) => f v -> ALGraph v e -> ALGraph v e
addVs vv g = foldl (flip addV) g vv

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

edgesFrom :: forall v e. Ord v => v -> ALGraph v e -> L.List (Edge v e)
edgesFrom v (ALGraph m) = fromMaybe L.Nil $ M.lookup v m

--

type DistToV v e = T.Tuple e v
type DistMap v e = M.Map v e
type PiMap v = M.Map v v
data Dijkstra v e = Dijkstra (OrdSeq.OrdSeq (DistToV v e)) (DistMap v e) (PiMap v)

shortestPath :: forall v e. (Ord v, Ord e, Monoid e) => v -> v -> ALGraph v e -> NE.Seq v
shortestPath v1 v2 g =
  case dij of Dijkstra _ _ pi' -> pathTo v2 pi' Nothing where
    q = OrdSeq.fromFoldable $ L.singleton $ T.Tuple mempty v1
    d = M.insert v1 mempty M.empty
    pi = M.empty
    dij = dijkstra (Dijkstra q d pi) g

dijkstra :: forall v e. (Ord v, Ord e, Monoid e) => Dijkstra v e -> ALGraph v e -> Dijkstra v e
dijkstra dij@(Dijkstra q d pi) g =
  case OrdSeq.popLeast q of
    Nothing -> dij
    Just (T.Tuple (T.Tuple d_to_v v) q') -> let
      nghs = edgesFrom v g
      dij' = foldl (relax d_to_v v) (Dijkstra q' d pi) nghs
      in dijkstra dij' g
      
relax :: forall v e. (Ord v, Ord e, Monoid e) => e -> v -> Dijkstra v e -> (T.Tuple e v) -> Dijkstra v e
relax d_to_v1 v1 dij@(Dijkstra _ d _) (T.Tuple e v2) =
  case M.lookup v2 d of
    Just d_to_v2 | d_to_v2 < d_to_v2_through_v1 -> dij
    current -> updateDistToV current d_to_v2_through_v1 v2 v1 dij      
  where d_to_v2_through_v1 = d_to_v1 ++ e
  
updateDistToV :: forall v e. (Ord v, Ord e) => Maybe e -> e -> v -> v -> Dijkstra v e -> Dijkstra v e
updateDistToV current_d_to_v new_d_to_v v new_parent (Dijkstra q d pi) =
  Dijkstra q'' (M.insert v new_d_to_v d) (M.insert v new_parent pi) where
    q' = case current_d_to_v of
      Nothing -> q
      Just cdtv -> OrdSeq.deleteAll (T.Tuple cdtv v) q
    q'' = OrdSeq.insert (T.Tuple new_d_to_v v) q'

pathTo :: forall v. Ord v => v -> PiMap v -> Maybe (NE.Seq v) -> NE.Seq v
pathTo v pi acc = case M.lookup v pi of
  Nothing -> cons v acc
  Just v2 -> pathTo v2 pi $ Just $ cons v acc
  where
    cons v Nothing = NE.singleton v
    cons v (Just s) = NE.cons v s
