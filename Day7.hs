module Day7 (title, part1, part2) where

import Data.List
import Data.Function
import Text.Parsec
import qualified Parser

findClosest:: ([Int] -> Int -> (Int, Int)) -> [Int] -> (Int, Int)
findClosest distanceF from = minimumBy (compare `on` snd) distances
  where 
    searchList = [(minimum from)..(maximum from)]
    distances = map (distanceF from) searchList

distance1:: [Int] -> Int -> (Int, Int)
distance1 from to = (to, sum $ map (abs.subtract to) from)

distance2:: [Int] -> Int -> (Int, Int)
distance2 from to = (to, sum $ map (distanceTo to) from)
  where distanceTo t f= (abs(t-f)+1)*abs(t-f) `div` 2

-- PARSER --
file = sepBy number (char ',')
number = read <$> many1 (oneOf "0123456789")

parseInput:: IO [Int]
parseInput = Parser.parseFile file "07.txt"

--export
title = "crab fuel optimization"
part1 = do
  inp <- parseInput
  print $ snd $ findClosest distance1 inp

part2 = do
  inp <- parseInput
  print $ snd $ findClosest distance2 inp
