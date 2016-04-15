module StopRoute
  ( Color
  , StopId()
  , newStopId
  , RouteFragment
  , Route
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

type Color = String

newtype StopId = StopId String

newStopId :: String -> StopId
newStopId = StopId

instance eqStopId :: Eq StopId where
  eq (StopId sid1) (StopId sid2) = eq sid1 sid2

instance ordsStopId :: Ord StopId where
  compare (StopId sid1) (StopId sid2) = compare sid1 sid2

type RouteFragment = NE.Seq StopId -- consecutive stops

type Route =
  { color :: Color
  -- start of fragment n+1 should be == to end of fragment n
  , fragments :: SQ.Seq RouteFragment 
  }

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
  
