module Day12 where

import Debug.Trace
import Data.List
import Data.Functor
import qualified Data.Map as M
import Text.Parsec
import qualified Parser
import Data.Maybe
import Data.Tuple

data Cave  = Start | End | Big String | Small String deriving (Show, Eq, Ord)
type Graph = M.Map Cave [Cave]

isSmall x = case x of
            Small _ -> True
            _       -> False

-- part1
edgesToGraph:: [(Cave, Cave)] -> Graph
edgesToGraph ls = M.fromListWith (++) listed
  where edges  = ls++map swap ls -- make edges bidirectional
        listed = map prep edges
        prep edge = case edge of
                    (End, _)      -> (End, [])    -- End leads nowhere 
                    (from, Start) -> (from, [])   -- nothing leads to Start
                    (from, to)    -> (from, [to]) -- make `to` appendable 

canVisit1:: Cave -> [Cave] -> Bool
canVisit1 cave@(Small _) visited = cave `notElem` visited
canVisit1 _ _ = True

--args:    graph     canVisit                 visited    start   paths
allPaths:: Graph -> (Cave -> [Cave]-> Bool) -> [Cave] -> Cave -> [[Cave]]
allPaths graph canVisit visited startingNode
  | not (canVisit startingNode visited) = []
  | End == startingNode                 = [[End]]
  | otherwise = case M.lookup startingNode graph of
        Nothing     -> []                
        Just neighb -> map (startingNode:) $ -- prepend to all paths in recursive call
          concatMap recurse neighb           -- recursion on all neighbors
   where recurse = allPaths graph canVisit (startingNode:visited)

-- part2
canVisit2 node visited = 
  case node of
    Small _ -> if hasTwo (filter isSmall visited)
               then node `notElem` visited
               else countOcc node visited < 2
    _       -> True
hasTwo ls = length (nub ls) /= length ls
countOcc x ls = length $ filter (==x) ls

-- ======parser======
file = edgesToGraph <$> many (edge <* eol) <* eof
edge = 
  do fst <- node
     char '-'
     snd <- node
     return (fst, snd)
node = try (string "start") $> Start
   <|> try (string "end")   $> End
   <|> Big   <$> count 2 upper
   <|> Small <$> count 2 lower
eol = char '\n'

parseInput:: IO Graph
parseInput = Parser.parseFile file "12.txt"

part1 = do
  inp <- parseInput
  let res = allPaths inp canVisit1 [] Start
  print $ length res

part2 = do
  inp <- parseInput
  let res = allPaths inp canVisit2 [] Start
  print $ length res
