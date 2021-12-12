module Main where

import System.TimeIt
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5
import qualified Day6
import qualified Day7
import qualified Day8
import qualified Day9
import qualified Day10
import qualified Day11
import qualified Day12

printPart num = putStr $ "⭐ Part " ++ show num ++ ": "

printDay num title part1 part2 = do
  putStrLn $ "Day "++ show num ++": "++title++"\n"++replicate 30 '-'
  printPart 1 
  timeIt part1
  printPart 2
  timeIt part2
  putStr "\n"

main = do
  putStrLn "✨✨Advent of Code solutions✨✨"
  putStr "\n"
  printDay 2  Day2.title Day2.part1 Day2.part2
  printDay 3  Day3.title Day3.part1 Day3.part2
  printDay 4  Day4.title Day4.part1 Day4.part2
  printDay 5  Day5.title Day5.part1 Day5.part2
  printDay 6  Day6.title Day6.part1 Day6.part2
  printDay 7  Day7.title Day7.part1 Day7.part2
  printDay 8  Day8.title Day8.part1 Day8.part2
  printDay 9  Day9.title Day9.part1 Day9.part2
  printDay 10 Day10.title Day10.part1 Day10.part2
  printDay 11 Day11.title Day11.part1 Day11.part2
  printDay 12 Day12.title Day12.part1 Day12.part2
