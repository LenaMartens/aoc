module Main where

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

divider num = replicate 7 '-' ++"day "++ show num ++ replicate 7 '-'
printDivider num = putStrLn $ divider num
main = do
  printDivider 2
  Day2.part1
  Day2.part2
  printDivider 3
  Day3.part2
  printDivider 4
  Day4.part1
  Day4.part2
  printDivider 5
  Day5.part1
  Day5.part2
  printDivider 6
  Day6.part1
  Day6.part2
  printDivider 7
  Day7.part1
  Day7.part2
  printDivider 8
  Day8.part1
  Day8.part2
  printDivider 9
  Day9.part1
  Day9.part2
  printDivider 10
  Day10.part1
  Day10.part2
  printDivider 11
  Day11.part1
  Day11.part2
  printDivider 12
  Day12.part1
  Day12.part2
