import System.IO
import System.Environment
import qualified Data.Set as S
import Data.Set (Set)
import qualified Data.Map as M
import Data.Map (Map)
import Data.List (intercalate, find)
import AStar

type Coord = (Double, Double)

parse :: String -> Map Coord (Set (String, Coord))
parse string = M.fromListWith S.union $ map (parseEdge . words) . filter ((>0) . length) . lines $ string
  where parseEdge :: [String] -> (Coord, Set (String, Coord))
        parseEdge [x1, y1, label, x2, y2] = ((read x1, read y1), S.singleton (label, (read x2, read y2)))
        parseEdge w = error $ "Invalid edge: " ++ (unwords w)

findJunctions :: Map Coord (Set (String, Coord)) -> String -> String -> Set Coord
findJunctions graph label1 label2 = S.intersection (candidates label1) (candidates label2)
  where candidates label = maybe S.empty id $ M.lookup label $ junctions
        junctions = M.fromListWith S.union $ foldl (++) [] $ map transform $ M.toList graph
          where transform (state1, edges) = map (\(label, state2) -> (label, S.fromList [state1, state2])) $ S.toList edges

findJunction :: Map Coord (Set (String, Coord)) -> String -> String -> Either String Coord
findJunction graph label1 label2
  | S.size junctions == 0 = Left ("Junction not found (" ++ label1 ++ "/" ++ label2 ++ ")")
  | S.size junctions > 1  = Left ("Ambiguous junction (" ++ label1 ++ "/" ++ label2 ++ ")")
  | otherwise             = Right (S.toList junctions !! 0)
  where junctions = findJunctions graph label1 label2

euclideanDistance :: Coord -> Coord -> Double
euclideanDistance (x1, y1) (x2, y2) = sqrt $ (x2 - x1)^2 + (y2 - y1)^2

formatOutput :: Coord -> String -> [(String, Coord)] -> String
formatOutput _ string [] = string
formatOutput from string ((label, to):edges) = formatOutput to string' edges
  where string' = (string ++ "\n-- " ++ label)

runProgram :: Map Coord (Set (String, Coord)) -> Either String Coord -> Either String Coord -> String
runProgram _ (Left message) _ = message
runProgram _ _ (Left message) = message
runProgram graph (Right start) (Right goal) = maybe "No solution!" (formatOutput start "Directions:") result
  where result = aStar expand distance heuristic goalTest start
          where expand state = maybe S.empty id (M.lookup state graph)
                distance x _ y = euclideanDistance x y
                heuristic x = euclideanDistance x goal
                goalTest x = (x == goal)

main = do
  args <- getArgs
  if length args < 5
    then putStrLn "Usage: ./router <file> <start 1st street> <start 2nd street> <goal 1st street> <goal 2nd street>"
    else do
      contents <- readFile $ args !! 0
      let graph = parse contents
          findJunction' = findJunction graph
          start = findJunction' (args !! 1) (args !! 2)
          goal = findJunction' (args !! 3) (args !! 4)
      putStrLn $ runProgram graph start goal