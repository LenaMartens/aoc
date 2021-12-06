module Main where

import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5

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
