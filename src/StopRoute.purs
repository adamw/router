module StopRoute
  ( Color(Color)
  , firstColor
  , nextColor
  , StopId()
  , newStopId
  , RouteFragment
  , Route
  , emptyRoute
  , routeContains
  , firstStop  
  , lastStop
  , addFragment
  , removeLastFragment
  , firstFragmentStop
  , lastFragmentStop
  , roads
  ) where

import Prelude
import Data.Sequence as SQ
import Data.Sequence.NonEmpty as NE
import Data.Maybe
import Data.Pair
import Data.Tuple
import Data.Foldable(any)

-- color indexes are 1-based
newtype Color = Color Int

instance eqColor :: Eq Color where
  eq (Color c1) (Color c2) = eq c1 c2

instance ordColor :: Ord Color where
  compare (Color c1) (Color c2) = compare c1 c2

firstColor :: Color
firstColor = Color 1

nextColor :: Color -> Color
nextColor (Color n) = Color (n+1)

newtype StopId = StopId String

newStopId :: String -> StopId
newStopId = StopId

instance eqStopId :: Eq StopId where
  eq (StopId sid1) (StopId sid2) = eq sid1 sid2

instance ordStopId :: Ord StopId where
  compare (StopId sid1) (StopId sid2) = compare sid1 sid2

type RouteFragment = NE.Seq StopId -- consecutive stops

type Route =
  { color :: Color
  -- start of fragment n+1 should be == to end of fragment n
  , fragments :: SQ.Seq RouteFragment 
  }

emptyRoute :: Color -> Route
emptyRoute c = { color: c, fragments: SQ.empty }

routeContains :: StopId -> Route -> Boolean
routeContains s { fragments = f } = any (fragmentContains s) f 

firstStop :: Route -> Maybe StopId
firstStop r = NE.head <$> SQ.head r.fragments

lastStop :: Route -> Maybe StopId
lastStop r = NE.last <$> SQ.last r.fragments

addFragment :: RouteFragment -> Route -> Route
addFragment rf r = r { fragments = SQ.cons rf r.fragments }

removeLastFragment :: Route -> Route
removeLastFragment r@{ fragments = f } = r { fragments = fromMaybe f $ SQ.init f }

fragmentContains :: StopId -> RouteFragment -> Boolean
fragmentContains s rf = any (eq s) rf

firstFragmentStop :: RouteFragment -> StopId
firstFragmentStop rf = NE.head rf

lastFragmentStop :: RouteFragment -> StopId
lastFragmentStop rf = NE.last rf

roads :: RouteFragment -> SQ.Seq (Pair StopId)
roads rf = let
  ht = NE.uncons rf
  rds prev acc Nothing = acc
  rds prev acc (Just (Tuple curr tail)) = rds curr (SQ.snoc acc (Pair prev curr)) (SQ.uncons tail)
  in rds (fst ht) SQ.empty (SQ.uncons $ snd ht)
  
