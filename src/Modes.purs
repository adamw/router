module Modes where

import Prelude

data Mode = EditorMode | AssignmentMode | SimulationMode

derive instance eqMode :: Eq Mode 

instance showMode :: Show Mode where
  show EditorMode = "Editor"
  show AssignmentMode = "Assignment"
  show SimulationMode = "Simulation"

allModes :: Array Mode
allModes = [ EditorMode, AssignmentMode, SimulationMode ]

modeEnabled :: Mode -> Mode -> Boolean
modeEnabled x y | x == y = true
modeEnabled EditorMode AssignmentMode = true
modeEnabled AssignmentMode EditorMode = true
modeEnabled AssignmentMode SimulationMode = true
modeEnabled SimulationMode _ = true
modeEnabled _ _ = false
