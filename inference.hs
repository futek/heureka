import System.IO
import System.Environment
import qualified Data.Set as S
import Data.Set (Set)
import Data.List (find, intercalate)
import Control.Applicative ((<$>), (<*>))
import Text.ParserCombinators.Parsec
import AStar

data Literal a = Negative a | Positive a deriving (Eq, Ord)
type Clause a = Set (Literal a)

instance (Show a) => Show (Literal a) where
  show (Negative a) = '¬' : show a
  show (Positive a) = show a

complement :: Literal a -> Literal a
complement (Negative a) = Positive a
complement (Positive a) = Negative a

negateClause :: (Ord a) => Clause a -> Set (Clause a)
negateClause = S.map (S.singleton . complement)

rpr :: (Ord a) => Clause a -> Clause a -> Set (Clause a)
rpr a b = S.map (\(Just c) -> c) $ S.delete Nothing $ S.map removePair union
  where union = a `S.union` b
        removePair l
          | (complement l) `S.member` union = Just $ S.delete (complement l) $ S.delete l $ union
          | otherwise                       = Nothing

prove :: (Ord a, Show a) => Set (Clause a) -> Clause a -> Maybe (Clause a, [(Clause a, Clause a)])
prove kb a = maybe Nothing (\(start, Just x) -> Just (start, x)) result
  where result = find (\(_, search) -> maybe False (\_ -> True) search) searches
        searches = map (\start -> (start, aStar expand distance heuristic goalTest start)) (S.toList negation)

        expand x = S.foldr S.union S.empty $ S.map (\y -> S.map (\c -> (y, c)) $ rpr x y) kb'
        distance _ _ _ = 1
        heuristic = S.size
        goalTest = S.null

        negation = negateClause a
        kb' = kb `S.union` negation

readKB :: String -> Set (Clause String)
readKB input = either (\pe -> error ("Error parsing knowledge base:\n" ++ show pe)) id (parse kb "" input)
  where kb = S.fromList <$> endBy (spaces >> clauseParser) (char '\n')

readClause :: String -> Clause String
readClause input = either (\pe -> error ("Error parsing clause:\n" ++ show pe)) id (parse clauseParser "" input)

clauseParser :: GenParser Char st (Clause String)
clauseParser = S.fromList <$> (try ifrule <|> disjunction)
  where ifrule = (\x xs -> x : map complement xs) <$> literal <*> (string " IF " >> conjunction)
        conjunction = sepBy1 literal (string " AND ")
        disjunction = sepBy1 literal (string " OR ")
        literal = negative <|> positive
        negative = complement <$> (string "NOT " >> positive)
        positive = Positive <$> many1 (noneOf "\n ")

showSolution :: (Show a) => (Clause a, [(Clause a, Clause a)]) -> String
showSolution (start, path) = showClause start ++ output
  where output = foldl1 (++) $ map (\(x, y) -> ", " ++ (showClause x) ++ " ⊢ " ++ (showClause y) ++ "\n" ++ (showClause y)) path

showClause :: (Show a) => Clause a -> String
showClause clause
  | S.null clause = "☐"
  | otherwise     = (filter notQuote . intercalate " ∨ " . map show . S.toList) clause
  where notQuote c = not $ c `elem` "\"'"

main = do
  args <- getArgs
  if length args < 2
    then putStrLn "Usage: ./inference <kb-file> <clause>"
    else do
      contents <- readFile $ args !! 0
      let kb = readKB contents
          a = readClause (args !! 1)
          result = prove kb a
      putStrLn $ maybe ("KB ⊭ " ++ showClause a) showSolution $ result
