{-# LANGUAGE ParallelListComp #-}
module Day15 (title, part1, part2) where

import Data.List
import Text.Parsec
import qualified Parser
import Data.Char (digitToInt)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (isJust, mapMaybe, fromJust)
import Data.Graph.AStar
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS

-- part1
enumerate:: [[a]] -> Map (Int, Int) a
enumerate grid = M.fromList $ concat [[((x, y), a) | a <- row    | y <- [0..]]
                               | row <- grid | x <- [0..]]

mysum ls = sum ls 

neighbs:: (Int, Int) -> [(Int, Int)]
neighbs (x, y) = [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]

search:: Map (Int, Int) Int -> (Int, Int) -> (Int, Int) -> Maybe [(Int, Int)]
search grid start end = aStar (getNeighbors grid) (getTravelCost grid) (manhattan end) (==end) start

manhattan:: (Int, Int) -> (Int, Int) -> Int
-- manhattan (endX, endY) (x, y) = (endX-x)+(endY-y)
manhattan _ _ = 0

getTravelCost:: Map (Int, Int) Int -> (Int, Int) -> (Int, Int) -> Int
getTravelCost grid _ coord = fromJust $ M.lookup coord grid

getNeighbors:: Map (Int, Int) Int -> (Int, Int) -> HashSet (Int, Int)
getNeighbors grid coord = HS.fromList $ filter (\x -> isJust (x `M.lookup` grid)) $ neighbs coord

-- part2
expand25x:: Map (Int, Int) Int -> (Int, Int) -> Map (Int, Int) Int
expand25x grid (width, height) =
  M.fromList $ concat [[((x+offsetX*height, y+offsetY*width), cost (x, y) (offsetX, offsetY))
                                      | x <- [0..(height-1)], offsetX<-[0..4]]
                                      | y <- [0..(width-1)],  offsetY<-[0..4]]
    where cost (x, y) (ox, oy)
            | a < 10 = a
            | otherwise = mod (a+1) 10
               where a = (ox+oy)+fromJust (M.lookup (x, y) grid) 
-- ======parser======
file = many (line <* eol) <* eof
line = many adigit
adigit = digitToInt <$> digit
eol = char '\n'

parseInput:: IO [[Int]]
parseInput = Parser.parseFile file "15.txt"

-- public export
title = "template"

bounds grid = (length grid-1, length (head grid)-1)
part1 = do
  inp <- parseInput
  let grid = enumerate inp
  let bestPath = search grid (0, 0) (bounds inp)
  return $ sum $ map (fromJust.(`M.lookup` grid)) $ fromJust bestPath

part2 = do
  inp <- parseInput
  let (width, height) = (length inp, length (head inp))
  let grid = expand25x (enumerate inp) (width, height)
  let (width, height) = (length inp, length (head inp))
  let (expandedW, expandedH) =(5*width-1, 5*height-1)
  let bestPath = search grid (0, 0) (expandedW, expandedH)
  return $ sum $ map (fromJust.(`M.lookup` grid)) $ fromJust bestPath
