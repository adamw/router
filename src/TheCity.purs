module TheCity (theCity) where

import Prelude
import City
import StopRoute (newStopId)

s1 = newStopId "s1"
s2 = newStopId "s2"
s3 = newStopId "s3"
s4 = newStopId "s4"

theCity :: City
theCity = empty 100.0 100.0 #
  addStop 20.0 20.0 s1 #
  addStop 70.0 30.0 s2 #
  addStop 10.0 50.0 s3 #
  addStop 50.0 90.0 s4 #
  addRoad s1 s2 #
  addRoad s2 s3 #
  addRoad s2 s4 #
  addRoad s3 s4
