module Editor
  ( EditorState(..)
  , EditedRoute
  , Editor
  , emptyEditor
  , selectStop
  , candidateStop
  , removeLastStop
  , finishRoute
  , editRoute
  , deleteRoute
  , createMap
  , editorTooltip
  ) where

import Prelude
import Route
import Data.Sequence as SQ
import Data.Sequence.NonEmpty as NE
import Data.Set as S
import City (businesses, residents, City, routeFragment)
import Data.Foldable (find)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Sequence (filter)
import RoutesMap as RoutesMap

data EditorState
  = SelectFirst
  | FirstStopCandidate StopId
  | FirstStopSelected StopId
  | FragmentCandidate EditorState StopId StopId RouteFragment
  | SelectNext StopId
    
type EditedRoute =
  { route :: Route
  , state :: EditorState
  }

type Editor =
  { city :: City
  , routes :: Routes
  , editedRoute :: EditedRoute
  }

emptyEditor :: City -> Editor
emptyEditor c = { city: c, routes: SQ.empty, editedRoute: er } where
  er = { route: emptyRoute initialRouteId, state: SelectFirst }

setState       st e = e { editedRoute = e.editedRoute { state = st } }
setEditedRoute r  e = e { editedRoute = e.editedRoute { route = r  } }

selectStop :: StopId -> Editor -> Editor
selectStop = let
  addFragmentBetween s1 s2 e = let 
    r' = addFragment (routeFragment s1 s2 e.city) e.editedRoute.route
    in setState (SelectNext s2) $ setEditedRoute r' e
  whenRouteEmpty s = setState (FirstStopSelected s)
  whenChosenIsFirst s1 s2 = finishRoute <<< (addFragmentBetween s1 s2)
  whenChosenIsNew = addFragmentBetween
  in chooseStop whenRouteEmpty whenChosenIsFirst whenChosenIsNew

candidateStop :: Maybe StopId -> Editor -> Editor
candidateStop Nothing e =
  case e.editedRoute.state of
    FirstStopCandidate _         -> setState SelectFirst e
    FragmentCandidate prev _ _ _ -> setState prev e
    _                            -> e
candidateStop (Just s) e = let
  candidateFragmentBetween s1 s2 e = let
    rf = routeFragment s1 s2 e.city
    newState = FragmentCandidate e.editedRoute.state s1 s2 rf
    in setState newState e
  whenRouteEmpty s = setState (FirstStopCandidate s)
  whenChosenIsFirst = candidateFragmentBetween
  whenChosenIsNew = candidateFragmentBetween
  in chooseStop whenRouteEmpty whenChosenIsFirst whenChosenIsNew s e

chooseStop whenRouteEmpty whenChosenIsFirst whenChosenIsNew s e@{ editedRoute: { route: r } } =
  let
    chooseFragment from = if (isFirstStop r s) && (from /= s)
      then whenChosenIsFirst from s e
      else if (routeContains s r) || (from == s) then e else whenChosenIsNew from s e
  in case e.editedRoute.state of
    SelectFirst -> whenRouteEmpty s e
    FirstStopCandidate _ -> whenRouteEmpty s e
    FirstStopSelected from -> chooseFragment from
    FragmentCandidate _ from _ _ -> chooseFragment from
    SelectNext last -> chooseFragment last

finishRoute :: Editor -> Editor
finishRoute e@{ routes: rs } = if isEmpty e.editedRoute.route
  then e
  else e { routes = rs', editedRoute = er' } where
    rs' = SQ.snoc rs e.editedRoute.route
    routeId' = nextRouteId (SQ.cons e.editedRoute.route.routeId ((_.routeId) <$> e.routes))
    er' = { route: emptyRoute routeId', state: SelectFirst }

removeLastStop :: Editor -> Editor
removeLastStop e = setState s' <<< setEditedRoute r' $ e where
  r' = removeLastFragment e.editedRoute.route
  s' = case lastStop r' of
    Just ls -> SelectNext ls
    -- if there's no last stop, it was the last fragment;
    -- new last stop is then the first stop (if any)
    Nothing -> case firstStop e.editedRoute.route of
      Just fs -> FirstStopSelected fs
      Nothing -> SelectFirst

editRoute :: RouteId -> Editor -> Editor
editRoute routeId e = withRouteRemoved routeId e doEdit where
  doEdit e' r = let
    -- if the route is circular, before editing we remove the last fragment (as we don't allow
    -- adding anything after the circle is complete)
    r' = if isCircular r then removeLastFragment r else r
    state = maybe SelectFirst SelectNext $ lastStop r' in
    setState state <<< setEditedRoute r' $ e'

deleteRoute :: RouteId -> Editor -> Editor
deleteRoute routeId e = withRouteRemoved routeId e (\e' r -> e')

withRouteRemoved routeId e f = let
  maybeRoute = find (\r -> r.routeId == routeId) e.routes
  withoutRoute = e { routes = filter (\r -> r.routeId /= routeId) e.routes }
  in maybe e (\r -> f withoutRoute r) maybeRoute
  
createMap :: Editor -> RoutesMap.RoutesMap
createMap e = RoutesMap.create routes (selectedStops e) where
  baseEditedRoute = e.editedRoute.route
  lastFragment = case e.editedRoute.state of
    FirstStopCandidate s -> Just $ NE.singleton s
    FirstStopSelected s  -> Just $ NE.singleton s
    FragmentCandidate _ _ _ rf -> Just rf
    _ -> Nothing
  editedRoute = maybe baseEditedRoute (\rf -> addFragment rf baseEditedRoute) lastFragment
  routes = SQ.cons editedRoute e.routes
  
selectedStops :: Editor -> S.Set StopId
selectedStops e =
  case e.editedRoute.state of
    FirstStopCandidate s        -> S.singleton s
    FirstStopSelected s         -> S.singleton s
    FragmentCandidate _ s1 s2 _ -> S.fromFoldable [ s1, s2 ]
    SelectNext last             -> S.singleton last
    _                           -> S.empty

editorTooltip :: Editor -> String
editorTooltip e = let
  state = e.editedRoute.state
  editorMsg SelectFirst                 = "Select first stop"
  editorMsg (FirstStopCandidate _)      = "Tap to select first stop"
  editorMsg (FirstStopSelected _)       = "First stop selected"
  editorMsg (FragmentCandidate _ _ _ _) = "Tap to add route fragment"
  editorMsg (SelectNext s)              = "Select next stop"
  showStop (FirstStopCandidate s)      = Just s
  showStop (FirstStopSelected s)       = Just s
  showStop (FragmentCandidate _ _ s _) = Just s
  showStop (SelectNext s)              = Just s
  showStop _                           = Nothing
  stopMsg s = "; "
              <> (show s)
              <> ", residents: "
              <> (show $ residents s e.city)
              <> ", businesses: "
              <> (show $ businesses s e.city)
  in (editorMsg state) <> (fromMaybe "" $ stopMsg <$> showStop state)
