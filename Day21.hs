{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Day21 (title, part1, part2) where

import Data.List ( group, sort )
import Data.List.Split ( chunksOf )
import Control.Monad.Memo (MonadMemo (memo), startEvalMemo, replicateM)

-- part1

deterministicRolls = chunksOf 3 [1..]

move start roll = 1+((new-1) `mod` 10)
  where new = start + sum roll

play:: (Integer, Integer) -> (Integer, Integer) -> Integer -> Integer
play (pos1, pos2) (score1, score2) rollNum
 | score1 >= 1000 = rollNum*score2*3
 | score2 >= 1000 = rollNum*score1*3
 | otherwise      = play (pos2, newPos) (score2, score1+newPos) (rollNum+1)
   where roll   = deterministicRolls!!fromIntegral rollNum
         newPos = move pos1 roll

-- part2
win = 21

sumTup (x, y) (a, b) = (x+a, y+b)

playQuantum:: (Integer, Integer) -> [Integer] -> Integer -> Int -> (Integer, Integer)
playQuantum (score1, score2) pos@[pos1, pos2] n0 turn
  | score1 >= win = (n0, 0)
  | score2 >= win = (0, n0)
  | otherwise     = foldl sumTup (0, 0) recurse
    where new n   = move (pos!!turn) [n]
          recurse = map quantumRoll dedup
          quantumRoll (n, roll) = playQuantum nextScore nextPos (n0*n) nextTurn
            where nextTurn  = (turn+1) `mod` 2
                  getMove   = new roll
                  (nextScore, nextPos) = case turn of
                                           0 -> ((score1+getMove, score2), [getMove, pos2])
                                           1 -> ((score1, score2+getMove), [pos1, getMove])

dedup:: [(Integer, Integer)]
dedup = map (\ls -> (toInteger (length ls), head ls)) $ group.sort.map sum $ replicateM 3 [1, 2, 3]
testinp   = (4, 8)
actualinp = (4, 3)

-- public export
title = "quantum die"

part1:: IO Integer
part1 = do
  let inp = actualinp
  return $ play inp (0, 0) 0

part2:: (Integer, Integer)
part2 = playQuantum (0, 0) [4, 3] 1 0
