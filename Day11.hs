{-# LANGUAGE ParallelListComp #-}
module Day11 (title, part1, part2) where

import Data.List
import Data.Char
import Data.Maybe
import qualified Parser
import Text.Parsec

-- =========part 1=========

countTens slice = sum $ map (length.filter (==10)) slice

reset x
  | x > 9    = 0
  | otherwise = x

flash:: Int -> [[Int]] -> (Int, [[Int]])
flash count grid
  -- propagate remaining to-be-flashed and count them
  | any (elem 10) grid = flash (count+countTens grid) $ propagateFlashes grid
  -- no more to-be-flashed
  | otherwise          = (count, map (map reset) grid)

propagateFlashes grid = n
  where n = [[curb a (countTens (slice2d (x, y) grid))
              | a   <- row  | y <- [0..]]
              | row <- grid | x <- [0..]]
        curb num tens = if num < 10
                         then min (num+tens) 10 -- will flash in next iteration
                         else 11                -- already flashed so advance beyond flash

slice2d:: (Int, Int) -> [[a]] -> [[a]]
slice2d (x, y) grid = map (slice y) (slice x grid)

-- slices out one before and after index x
slice x ls = take (3-s x) $ drop (x-1) ls

-- needed for slicing at index 0
s 0 = 1
s _ = 0

step:: (Int, [[Int]]) -> (Int, [[Int]])
step (count, grid) = (count+countFlashes, newGrid)
  where (countFlashes, newGrid) = flash 0 (map (map (+1)) grid)

steps n grid = iterate step (0, grid)!!n

-- =========part 2=========

stepGrids grid = newGrid
  where (_, newGrid) = flash 0 (map (map (+1)) grid)

firstSync grid = findIndex synced (iterate stepGrids grid)

synced x = sum (map sum x) == 0

-- pretty print 2d grid for debugging
divider = replicate 10 '-' ++ "\n"
pprint:: [[Int]] -> String
pprint grid = divider ++ concatMap ((++"\n").show) grid ++ "\n"

-- ======parser======
file = many (many number <* eol) <* eof
number = digitToInt <$> digit
eol = char '\n'

parseInput = Parser.parseFile file "11.txt"

-- export
title = "flashing octupi"

part1 = do fst . steps 100 <$> parseInput

part2 = do fromJust . firstSync <$> parseInput
