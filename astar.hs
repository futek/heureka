module AStar (aStar) where

import Data.Maybe (isNothing)
import qualified Data.Map as M
import Data.Map (Map)
import qualified Data.Set as S
import Data.Set (Set)
import qualified Data.PSQueue as Q
import Data.PSQueue (PSQ, Binding(..))

aStar :: (Ord a, Ord c, Num c) => (a -> Set (b, a)) -> (a -> b -> a -> c) -> (a -> c) -> (a -> Bool) -> a -> Maybe [(b, a)]
aStar expand distance heuristic goalTest start = aStar' expand distance heuristic goalTest frontier explored history
  where frontier = Q.singleton start (heuristic start)
        explored = S.empty
        history = M.empty

aStar' :: (Ord a, Ord c, Num c) => (a -> Set (b, a)) -> (a -> b -> a -> c) -> (a -> c) -> (a -> Bool) -> PSQ a c -> Set a -> Map a (b, a) -> Maybe [(b, a)]
aStar' expand distance heuristic goalTest frontier explored history
  | Q.null frontier = Nothing
  | goalTest state  = Just (buildSolution history state (M.lookup state history) [])
  | otherwise       = aStar' expand distance heuristic goalTest frontier' explored' history'
  where (Just (state :-> cost)) = Q.findMin frontier
        frontier' = foldl (\acc (n, c) -> Q.insert n c acc) (Q.deleteMin frontier) (map prepare expansion)
          where prepare (l, s) = (s, cost - heuristic state + distance state l s + heuristic s)
        explored' = S.insert state explored
        history' = foldl (\acc (l, s) -> M.insert s (l, state) acc) history expansion
        expansion = filter predicate . S.toList $ expand state
          where predicate (_, s) = not (S.member s explored) && isNothing (Q.lookup s frontier)

buildSolution :: (Ord a) => Map a (b, a) -> a -> Maybe (b, a) -> [(b, a)] -> [(b, a)]
buildSolution _ _ Nothing solution = solution
buildSolution history state (Just (l, s)) solution = buildSolution history s (M.lookup s history) ((l, state):solution)
