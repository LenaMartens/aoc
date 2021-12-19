module Day16 (title, part1, part2) where

import Numeric (readHex, showIntAtBase, readInt)
import Data.Char (intToDigit, digitToInt)
import Data.List
import Text.Parsec
import qualified Parser

data Packet = Literal Int Int           -- version + value
            | Operator Int Int [Packet] -- version + op id + subpackets
            deriving (Show)

-- part1
parseWith:: Parsec String () a -> String -> a
parseWith parser inp = case parse parser "<parsing with>" inp of
                       Left err -> error (show err)
                       Right x  -> x
parsePacket:: String -> Packet
parsePacket = parseWith packet

-- ints <> binary <> hex
hexToInt ch = fst.head $ readHex [ch] :: Int
intToBin x  = padded
 where unpadded = showIntAtBase 2 intToDigit x ""
       padded   = replicate (4-length unpadded)'0' ++ unpadded
readBin str = fst.head $ readInt 2 (`elem` "01") digitToInt str
binString = concatMap (intToBin . hexToInt)

-- packet parser
packet = choice [try literal, try operator]

version = readBin <$> count 3 bin
bin = oneOf "01"

literal = do
  ver <- version
  string "100"  -- id=4
  mids <- many (char '1' *> count 4 bin)
  end  <- char '0' *> count 4 bin
  return $ Literal ver $ readBin $ concat (mids++[end])

operator = do
  ver <- version
  id  <- readBin <$> count 3 bin
  subPackets <- stringLength <|> packetamount
  return $ Operator ver id subPackets

stringLength = do
  char '0'
  len <- readBin <$> count 15 bin
  parseWith (many packet)<$> count len bin

packetamount = do
  char '1'
  amount <- readBin <$> count 11 bin
  count amount packet

versionSum:: Packet -> Int
versionSum (Literal v _)     = v
versionSum (Operator v _ sp) = v + sum (map versionSum sp)

-- part2
op:: Int -> ([Int] -> Int)
op 0 = sum
op 1 = product
op 2 = minimum
op 3 = maximum
op 5 = \ls -> toInt $ ls!!0 > ls!!1
op 6 = \ls -> toInt $ ls!!0 < ls!!1
op 7 = \ls -> toInt $ ls!!0 == ls!!1
op x = error $ show x

toInt :: Bool -> Int
toInt True  = 1
toInt False = 0

eval:: Packet -> Int
eval (Literal  _ val)        = val
eval (Operator _ opCode sub) = op opCode $ map eval sub

-- ======(file)parser======
parseInput = Parser.parseFile (many hexDigit <* eol) "16.txt"
eol = char '\n'

-- public export
title = "parsec, more like...parsepackets"

part1 = versionSum . parsePacket . binString <$> parseInput

part2 = eval . parsePacket . binString <$> parseInput
