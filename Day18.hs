module Day18 (title, part1, part2) where

import Data.List ()
import Text.Parsec ( many, char, digit, choice, count, eof )
import qualified Parser

data Number = Pair Number Number | Regular Int deriving (Show)

-- part1
add:: Number -> Number -> Number
add x y = redux $ Pair x y

redux :: Number -> Number
redux (Pair left right) = error "not implemented"

-- part2
-- ======parser======
file = many (number <* eol) <* eof
number = choice [pair, regular]
pair   = do
           char '['
           f <- number
           char ','
           s <- number
           char ']'
           return $ Pair f s

regular = Regular . read <$> count 1 digit
eol = char '\n'

parseInput:: IO [Number]
parseInput = Parser.parseFile file "18.txt"

-- public export
title = "snail math"

part1 = do
  inp <- parseInput
  print inp

part2 = do
  parseInput
