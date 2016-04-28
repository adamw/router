module TheCity (theCity) where

import Prelude
import City
import Route (newStopId)

s1 = newStopId "s1"
s2 = newStopId "s2"
s3 = newStopId "s3"
s4 = newStopId "s4"

theCity :: City
theCity = empty 300.0 300.0 #
  addStop  20.0 80.0 s1 #
  addStop 170.0 130.0 s2 #
  addStop  30.0 250.0 s3 #
  addStop 250.0 290.0 s4 #
  addRoad s1 s2 #
  addRoad s2 s3 #
  addRoad s2 s4 #
  addRoad s3 s4
