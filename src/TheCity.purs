module TheCity (city, buses) where

import Prelude
import City
import Route (newStopId)

s1 = newStopId "s1"
s2 = newStopId "s2"
s3 = newStopId "s3"
s4 = newStopId "s4"
s5 = newStopId "s5"
s6 = newStopId "s6"

city :: City
city = empty 400.0 400.0 #
  addStop  50.0 80.0  s1 #
  addStop 170.0 130.0 s2 #
  addStop  40.0 250.0 s3 #
  addStop 250.0 290.0 s4 #
  addStop 100.0 350.0 s5 #
  addStop 350.0 200.0 s6 #
  addRoad s1 s2 #
  addRoad s2 s3 #
  addRoad s2 s4 #
  addRoad s3 s4 #
  addRoad s4 s5 #
  addRoad s4 s6 #
  addRoad s2 s6 #
  setResidents s1 300 #
  setResidents s2 60  #
  setResidents s3 200 #
  setResidents s5 200 #
  setBusinesses s2 250 #
  setBusinesses s3 10  #
  setBusinesses s4 300 #
  setBusinesses s6 200

buses = 10
