module StopRoute
  ( Color(Color)
  , StopId()
  , newStopId
  , RouteFragment
  , Route
  , emptyRoute
  , lastStop
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

-- color indexes are 1-based
newtype Color = Color Int

instance eqColor :: Eq Color where
  eq (Color c1) (Color c2) = eq c1 c2

instance ordColor :: Ord Color where
  compare (Color c1) (Color c2) = compare c1 c2

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

lastStop :: Route -> Maybe StopId
lastStop r = NE.last <$> SQ.last r.fragments

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
  
