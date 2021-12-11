module Day5  where

import Data.List
import Text.Parsec hiding (space)
import qualified Parser

isStraigthLine:: Vent -> Bool
isStraigthLine ((x0, y0), (x1, y1)) = (x0==x1) || (y0==y1)

expand:: Vent -> [Coord]
expand ((x0, y0), (x1, y1))
      | x0==x1 = zip (repeat x0) yRange
      | y0==y1 = zip xRange (repeat y0)
      | otherwise = zip xRange yRange
  where 
    fwdXRange = [(min x0 x1)..(max x0 x1)]
    fwdYRange = [(min y0 y1)..(max y0 y1)]
    xRange
      | x0 > x1 = reverse fwdXRange
      | otherwise = fwdXRange
    yRange
      | y0 > y1 = reverse fwdYRange
      | otherwise = fwdYRange


groupOccurences:: Ord a => [a] -> [[a]]
groupOccurences = group . sort

countDanger ls= length $ filter (>1) $ map length ls

-- types
type Vent = (Coord, Coord)
type Coord = (Int, Int)

-- PARSER --
file = do 
  vents <- many1 vent
  eof
  return vents
vent = do
  co1 <- coord
  string " -> "
  co2 <- coord
  char '\n'
  return (co1, co2)
coord = do
  x <- number
  char ','
  y <- number
  return (x, y)
number = read <$> many1 (oneOf "0123456789")

parseInput:: IO [Vent]
parseInput = Parser.parseFile file "05.txt"

-- solutions
part1 = do
  inp <- parseInput
  let coveredCoords = concatMap expand $ filter isStraigthLine inp
  print $ countDanger $ groupOccurences coveredCoords

part2 = do
  inp <- parseInput
  let coveredCoords = concatMap expand inp
  print $ countDanger $ groupOccurences coveredCoords
