module Test.Main (main) where

import Prelude

import Test.Unit.Main (runTest)

import Test.Data.ALGraph

main = runTest do
  shortestPathTest
