module Day1 (title, part1, part2, exp1, exp2) where

import Data.List
import Text.Parsec
import qualified Parser

-- part 1
countIncrease acc (f, s) = if s > f then acc+1 else acc

-- ======parser======
file = many1 (number <* eol) <* eof
number = read <$> many1 (oneOf "0123456789")
eol = char '\n'

parseInput:: IO [Int]
parseInput = Parser.parseFile file "01.txt"

-- public export
title = "sonar sweep"

part1 = do
  inp <- parseInput
  return $ foldl countIncrease 0 $ zip inp $ tail inp
exp1 = 1226

part2 = do
  inp <- parseInput
  let windows = map (sum.take 3) $ tails inp
  return $ foldl countIncrease 0 $ zip windows $ tail windows
exp2 = 1252
