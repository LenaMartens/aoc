module Day13 (title, part1, part2) where

import Data.List
import Text.Parsec
import qualified Parser
import Data.Set (Set)
import qualified Data.Set as S
import Data.Functor (($>))
import Debug.Trace (trace)

data FoldInstr = X Int | Y Int
type Grid = Set (Int, Int)

-- part1
foldGrid:: Grid -> FoldInstr -> Grid
foldGrid grid foldInstr = S.map (foldUp foldInstr) grid
  where
  foldUp (Y f) (x, y) = (x, displacement y f)
  foldUp (X f) (x, y) = (displacement x f, y)
  displacement n f
    | n < f = n
    | otherwise = f-(n-f)

-- debugging (which coincidentally is part2!)
pprint:: Grid -> String
pprint grid = unlines gridStrings
  where gridStrings = [[toDot (x, y)| x <- [0..width]]| y <- [0..height]]
        (width, height) = bounds grid
        toDot (x, y)
          | (x, y) `elem` grid = '█'
          | otherwise          = '.'

bounds:: Set (Int, Int) -> (Int, Int)
bounds set = (foldl max 0 (S.map fst set),
              foldl max 0 (S.map snd set))

-- ======parser======
file = do
  dots <- many dot
  eol
  instrs <- many (choice [try foldInstrX, try foldInstrY])
  return (dots, instrs)

dot  = do
  fst <- number
  char ','
  snd <- number
  eol
  return (fst, snd)
foldInstrX = do
  string "fold along x="
  instr <- X <$> number
  eol
  return instr
foldInstrY = do
  string "fold along y="
  instr <- Y <$> number
  eol
  return instr
number = read <$> many (oneOf "0123456789")
eol = char '\n'

parseInput:: IO ([(Int, Int)], [FoldInstr])
parseInput = Parser.parseFile file "13.txt"

-- public export
title = "beta-fold"

part1 = do
  (grid, folds) <- parseInput
  return $ S.size $ foldGrid (S.fromList grid) $ head folds

part2 = do
  (grid, folds) <- parseInput
  putStr $ pprint $ foldl foldGrid (S.fromList grid) folds
