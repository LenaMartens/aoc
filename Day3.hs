module Day3 where

import Data.List
import Data.Char
import Text.Parsec
import qualified Parser

countOcc x = length . filter (x==)

filterBits:: ([Int]->Int) -> [[Int]] -> [Int] -> Int -> [Int]
filterBits _ [one] _ _ = one
filterBits f ls patternSoFar index = filterBits f filteredList newPattern (index+1)
  where 
    newPattern = patternSoFar ++ [f (transpose ls!!index)]
    filteredList = filter (isPrefixOf newPattern) ls

least:: [Int] -> Int
least ls
  | zeros <= ones = 0
  | ones < zeros = 1
  where
    zeros = countOcc 0 ls
    ones = length ls - zeros

most:: [Int] -> Int
most [] = 0
most ls
  | zeros > ones = 0
  | ones >= zeros = 1
  where
    zeros = countOcc 0 ls
    ones = length ls - zeros

unDigits base = foldl (\ a b -> a * base + b) 0
-- PARSER
file = many (line <* eol) <* eof
line = many binDigit
binDigit = digitToInt <$> oneOf "01"
eol = char '\n'

parseInput:: IO [[Int]]
parseInput = Parser.parseFile file "03.txt"

-- TODO: reconstruct part 1
part2 = do
  input <- parseInput
  let oxygen = unDigits 2 $ filterBits most  input [] 0
  let co2    = unDigits 2 $ filterBits least input [] 0
  print (oxygen*co2)
