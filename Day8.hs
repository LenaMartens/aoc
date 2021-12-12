module Day8 (title, part1, part2) where

import Data.Tuple
import qualified Data.Map as M
import Data.List
import Text.Parsec hiding (space)
import qualified Parser
import Data.Maybe (fromJust)
import Data.Foldable (Foldable(toList))

-- data
easyDisplays = [2, 3, 4, 7]
displays = ["abcefg", "cf", "acdeg", "acdfg", "bcdf", "abdfg", "abdefg", "acf", "abcdefg", "abcdfg"]

canonical:: String -> Int
canonical  "abcefg"   =  0
canonical  "cf"       =  1
canonical  "acdeg"    =  2
canonical  "acdfg"    =  3
canonical  "bcdf"     =  4
canonical  "abdfg"    =  5
canonical  "abdefg"   =  6
canonical  "acf"      =  7
canonical  "abcdefg"  =  8
canonical  "abcdfg"   =  9
canonical  _          = 666

-- part 1
easyNumber:: String -> Bool
easyNumber = flip elem easyDisplays . length

allEasyNumbers:: [FullReading] -> [String]
allEasyNumbers readings = filter easyNumber allOutputReadings
  where allOutputReadings = concatMap snd readings

-- part 2
allMappings:: [M.Map Char Char]
allMappings = map (M.fromList . zip ['a'..'g']) $ permutations ['a'..'g']

invert m = M.fromList invertedList
  where invertedList = map swap $ M.toList m

generateFaultyDisplays:: M.Map Char Char -> [String]
generateFaultyDisplays mapping = sort $ map (convert mapping) displays

convert:: M.Map Char Char -> String -> String
convert mapping = sort . map lookup
  where lookup x = fromJust $ M.lookup x mapping

findMatchingMapping:: DisplaySamples -> Maybe (M.Map Char Char)
findMatchingMapping readings = find (matching readings) allMappings

matching readings m = readings == generateFaultyDisplays m

search readings = findMatchingMapping $ sort $ map sort readings
getMapping = invert.fromJust.search

getNumber:: M.Map Char Char -> String -> Int
getNumber m o = canonical $ sort (convert m o)

unDigits:: [Int] -> Int
unDigits = foldl (\ a b -> a * 10 + b) 0

fullDecode:: FullReading -> Int
fullDecode (readings, outputs) = unDigits $ map (getNumber (getMapping readings)) outputs

-- types
type DisplaySamples = [String]
type OutputReading = [String]
type FullReading = (DisplaySamples, OutputReading)

-- PARSER --
file = many1 reading <* eof
reading = do
  first <- sampleReadings
  string "| "
  second <- outputReadings
  char '\n'
  return (first, second)
sampleReadings = many1 (agroup <* space)
outputReadings = sepBy1 agroup space
agroup = many1 (noneOf " \n|")
space = char ' '

parseInput:: IO [FullReading]
parseInput = Parser.parseFile file "08.txt"

--export
title = "faulty display decoding"
part1 = do length . allEasyNumbers <$> parseInput

part2 = do sum . map fullDecode <$> parseInput
