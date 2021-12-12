{-# LANGUAGE FlexibleContexts #-}
module Day6 (title, part1, part2) where

import Data.List
import qualified Data.Map as Map
import qualified Parser
import Text.Parsec

-- naive iteration, part 1
advanceFish 0 = 6
advanceFish x = x-1

advanceSchool school = newSchool
  where
    newFish   = length $ filter (==0) school
    newSchool = map advanceFish school ++ replicate newFish 8

advanceN n s = iterate advanceSchool s!!n

-- bucketing, part 2
countOcc x = length . filter (==x)

initialBuckets:: [Int] -> [Int]
initialBuckets fish = [countOcc i fish | i <- [0..8]]

advanceBuckets:: [Int] -> [Int]
advanceBuckets buckets = [newFish i| i <- [0..8]]
  where newFish 8 = head buckets
        newFish 6 = head buckets + buckets!!7
        newFish index = buckets!!(index+1)

-- PARSER --
file = sepBy number (char ',')
number = read <$> many1 (oneOf "0123456789")

parseInput:: IO [Int]
parseInput = Parser.parseFile file "06.txt"

-- solutions
title = "lanternfish generations"
part1 = do
  inp <- parseInput
  return $ sum $ iterate advanceBuckets (initialBuckets inp)!!80

part2 = do
  inp <- parseInput
  return $ sum $ iterate advanceBuckets (initialBuckets inp)!!256
