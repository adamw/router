module Test.Data.ALGraph (shortestPathTest) where

import Prelude

import Test.Unit (test)
import Test.Unit.Assert as Assert

import Data.ALGraph

import Data.List as L
import Data.Monoid.Additive

a n = Additive n

single_vertex :: ALGraph String (Additive Number)
single_vertex =
  addV "v1" $
  empty

two_vertices =
  addDirectedE "v1" "v2" (a 2.0) $
  addV "v1" $
  addV "v2" $
  empty

four_vertices =
  addDirectedE "v1" "v2" (a 3.0) $
  addDirectedE "v2" "v4" (a 3.0) $
  addDirectedE "v1" "v3" (a 2.0) $
  addDirectedE "v3" "v4" (a 1.0) $
  addDirectedE "v1" "v4" (a 5.0) $
  addVs [ "v1", "v2", "v3", "v4" ] $
  empty

shortestPathTest = test "shortest path" do
  Assert.equal (L.fromFoldable [ "v1" ]) $ shortestPath "v1" "v1" single_vertex
  Assert.equal (L.fromFoldable [ "v1" ]) $ shortestPath "v1" "v1" two_vertices
  Assert.equal (L.fromFoldable [ "v1", "v2" ]) $ shortestPath "v1" "v2" two_vertices
  Assert.equal (L.fromFoldable [ "v1", "v3", "v4" ]) $ shortestPath "v1" "v4" four_vertices
