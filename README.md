Always visible:
- Edit routes -> Assign buses -> Simulate (modes)
- footer
- some form of route map
- right-hand control panel

Dictionary:
- Stop
- Road - two connected stops
- RouteFragment - consecutive stops (connected by roads)
- Route - consecutive route fragments
- RouteMap - for each stop, routes that stop there, for each road, routes on the road
- mode: edit routes, assign buses, simulate
- Editor - route map editor
- BusMap: postition of each bus on the map

Actions:
- control: noop, animation frame
- switch mode
- modal
- tooltip
- route-map-specific: clik, hover stop
- mode-specific
  - editor: complete route, remove last stop, remove route, edit route

Components:
- Main
  - manages main loop, signals
  - draws common components
  - dispatches drawing & actions to mode-specific components (EditorMain etc.)
