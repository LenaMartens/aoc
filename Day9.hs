{-# LANGUAGE ParallelListComp #-}
module Day9 where

import Data.List
import Data.Function
import Text.Parsec
import Data.Char
import Parser

neighborIdx:: (Int, Int) -> (Int, Int) -> [(Int, Int)]
neighborIdx (boundx, boundy) (x, y) = [(x+xd, y+yd) | 
                                       xd <- [-1, 0, 1],
                                       yd <- [-1, 0, 1], 
                                       abs(xd+yd)==1,
                                       x+xd <= boundx, y+yd <= boundy,
                                       x+xd > -1, y+yd > -1]

bounds:: [[Int]] -> (Int, Int)
bounds grid = (length grid-1, length (head grid)-1)

-- part 1
danger:: [[Int]] -> Int -> (Int, Int) -> Int
danger grid curr coord
  | minimum (map lookup2D (neighborIdx (bounds grid) coord)) > curr = curr + 1
  | otherwise = 0
  where lookup2D (x, y) = grid!!x!!y 

allDanger grid = sum [sum [danger grid curr (x, y)
                           | curr <- row  | y <- [0..]]
                           | row  <- grid | x <- [0..]]

-- part2
allCoords grid = [(x, y)| x <- [0..length grid-1], y <- [0..length (head grid)-1]]
startSearch grid = findBassins grid [] (allCoords grid) []

findBassins:: [[Int]] -> [(Int, Int)] -> [(Int, Int)] -> [[(Int, Int)]] -> [[(Int, Int)]]
findBassins _ _ [] bassins = bassins
findBassins grid seen (curr@(x, y):rst) bassins = findBassins grid (seen++bassin) rst (bassins++[bassin])
  where bassin = if (curr `elem` seen) || (grid!!x!!y == 9)
                 then []
                 else expand grid [] [curr]

expand:: [[Int]] -> [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
expand _ seen [] = nub seen
expand grid seen (curr:rst) = expand grid (seen++[curr]) (rst++expansion)
 where (addCurr, expansion) = if curr `elem` seen
                         then ([], [])
                         else ([curr], [(x, y)| (x, y) <- neighborIdx (bounds grid) curr,
                            (x, y) `notElem` seen,
                            grid!!x!!y /= 9])

sumThreeLargest bassins = product $ take 3 $ reverse $ sort $ map length bassins
-- PARSER --
file = many (line <* eol) <* eof
line = many adigit
adigit = digitToInt <$> digit
eol = char '\n'

parseInput:: IO [[Int]]
parseInput = Parser.parseFile file "09.txt"

part1 = do
  inp <- parseInput
  print $ allDanger inp

part2 = do
  inp <- parseInput
  print $ sumThreeLargest $ startSearch inp
