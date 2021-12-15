{-# LANGUAGE BangPatterns #-}
module Main where

import System.TimeIt
import Text.Printf
import qualified Day1
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
import qualified Day14

printPart num part expected = do
  putStr $ "| Part " ++ show num ++ ": "
  -- TODO: fix timing (foiled by thunks!)
  (!time, !partRes) <- timeItT part
  -- puzzle result
  putStr (show partRes)
  -- spaces to justify next strs to the right of the table
  putStr $ replicate (19-length(show partRes))' '
  -- mili-seconds
  putStr $ printf "%.2f" (time*1000)++"ms | "
  -- as expected?
  putStr $ testStr partRes expected
  putStr "\n"

testStr res exp = if res == exp
                  then "⭐ |"
                  else "💔 |"++"\ESC[31m expected: "++show exp++"\ESC[0m"

tableTop = "✦"++replicate 40 '-'++"✦"

printDay num title part1 part2 exp1 exp2 = do
  putStrLn $ " Day "++ show num ++": "++title
  putStrLn tableTop 
  printPart 1 part1 exp1
  printPart 2 part2 exp2
  putStrLn tableTop 
  putStr "\n"

main = do
  putStrLn "✨✨✨Advent of Code solutions✨✨✨"
  putStr "\n"
  printDay 1  Day1.title  Day1.part1  Day1.part2  Day1.exp1 Day1.exp2 
  printDay 2  Day2.title  Day2.part1  Day2.part2  1484118 1463827010
  printDay 3  Day3.title  Day3.part1  Day3.part2  4191876 3414905
  printDay 4  Day4.title  Day4.part1  Day4.part2  67716   1830
  printDay 5  Day5.title  Day5.part1  Day5.part2  4993    21101
  printDay 6  Day6.title  Day6.part1  Day6.part2  396210  1770823541496
  printDay 7  Day7.title  Day7.part1  Day7.part2  333755  94017638
  printDay 8  Day8.title  Day8.part1  Day8.part2  539     1084606
  printDay 9  Day9.title  Day9.part1  Day9.part2  514     1103130
  printDay 10 Day10.title Day10.part1 Day10.part2 215229  1105996483
  printDay 11 Day11.title Day11.part1 Day11.part2 1735    400
  printDay 12 Day12.title Day12.part1 Day12.part2 3738    120506
  -- TODO: add Day13 (output is not an int!)
  printDay 14 Day14.title Day14.part1 Day14.part2 Day14.exp0 Day14.exp1
