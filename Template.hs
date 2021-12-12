module DayN (title, part1, part2) where

import Data.List
import Text.Parsec
import qualified Parser

-- part1
-- part2
-- ======parser======
file = many eol <* eof
eol = char '\n'

parseInput:: IO [String]
parseInput = Parser.parseFile file "N.txt"

-- public export
title = "template"

part1 = do
  inp <- parseInput
  print inp

part2 = do
  inp <- parseInput
  print inp
