{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Day14 (title, part1, part2, exp0, exp1) where

import Data.List
import Text.Parsec
import qualified Parser
import Data.Map (Map)
import qualified Data.Map as M

-- part1
insertBetween:: Map String String -> String -> String -> String
insertBetween _ acc []    = acc
insertBetween _ acc [one] = acc++[one]
insertBetween instrs acc (f:s:rst) = insertBetween instrs newAcc (s:rst)
  where newAcc = case M.lookup (f:[s]) instrs of
                   Nothing -> acc++[f]
                   Just b  -> acc++(f:b)

insertNTimes instrs start n = iterate (insertBetween instrs []) start!!n

score:: String -> Integer
score seq = most-least
  where occs = M.fromListWith (+) $ map (,1) seq
        sorted = sortOn snd $ M.toList occs
        least = snd (head sorted)
        most  = snd (last sorted)

-- part2
toHistogram:: String -> Map String Integer
toHistogram str = M.union pairHistogram originalCharCounts
  where pairs = zipWith (\x y -> [x, y]) str $ tail str
        pairHistogram = M.fromListWith (+) $ map (,1) pairs
        originalCharCounts = M.fromListWith (+) $ map ((,1) . (: [])) str

nextHistogram:: [(String, Char)] -> Map String Integer -> Map String Integer
nextHistogram instrs curr = advance curr instrs curr
 where
   advance m [] nm = nm
   advance m (instr:rst) nm = advance m rst (insertInMap m instr nm)

insertInMap:: Map String Integer -> (String, Char) -> Map String Integer -> Map String Integer
insertInMap m (f:[s], insrt) nm =
  case M.lookup [f, s] m of -- look-up in the previous iteration map
    Nothing -> nm
    Just 0  -> nm
    Just x  ->   M.insertWith (flip (-)) [f, s]     x  -- remove x of the lhs pair
               $ M.insertWith (+)        [insrt, s] x  -- add x of both generated pairs 
               $ M.insertWith (+)        [f, insrt] x
               $ M.insertWith (+)        [insrt]    x  -- add x to count of inserted charachter
                 nm  -- update the current-iteration map

insertNTimes2 instrs start n = iterate (nextHistogram instrs) start!!n

score2:: Map String Integer -> Integer
score2 m = most - least
  where charCount = M.filterWithKey (\k _ -> length k == 1) m
        sorted = sortOn snd $ M.toList charCount
        least = snd (head sorted)
        most  = snd (last sorted)

-- ======parser======
file = do
  start <- many upper
  count 2 eol
  insertions <- many insertion
  eof
  return (start, insertions)

insertion = do
  from <- count 2 upper
  string " -> "
  to <- upper
  eol
  return (from, to)

eol = char '\n'

parseInput:: IO (String, [(String, Char)])
parseInput = Parser.parseFile file "14.txt"

-- public export
title = "polymere expansion"

part1 = do
  (start, instrs) <- parseInput
  let seq = insertNTimes2 instrs (toHistogram start) 10
  return $ score2 seq
exp0 = 3555

part2 = do
  (start, instrs) <- parseInput
  let seq = insertNTimes2 instrs (toHistogram start) 40
  return $ score2 seq
exp1 = 4439442043739
