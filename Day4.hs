module Day4 (title, part1, part2) where

import Data.List
import Text.Parsec hiding (space)
import qualified Parser

hasWon:: TrackedBingoCard -> Bool
hasWon (t, _) = fullRow || fullColumn
  where
    fullRow = any and t
    fullColumn = any and (transpose t)

mark:: Int -> TrackedBingoCard -> TrackedBingoCard
mark calledNum (tracking, card) = (newTracking, card)
  where
    mask = map (map (==calledNum)) card
    newTracking = zipWith (zipWith (||)) tracking mask

initialTracking:: BingoCard -> TrackedBingoCard
initialTracking b = (map (map (const False)) b, b)

score:: TrackedBingoCard -> Int
score (t, c) = sum (concat filteredNumbers)
  where filteredNumbers = zipWith (zipWith filterNumber) t c

filterNumber:: Bool -> Int -> Int
filterNumber False x = x
filterNumber True x = 0

playBingo:: [Int] -> [TrackedBingoCard] -> (Int, TrackedBingoCard)
playBingo (cur:rs) cards =
  let updatedCards = map (mark cur) cards
  in case find hasWon updatedCards of
     Just winner -> (cur, winner)
     Nothing -> playBingo rs updatedCards

playBingoWithSquid:: [Int] -> [TrackedBingoCard] -> (Int, TrackedBingoCard)
playBingoWithSquid nums [card]    = playBingo nums [card]
playBingoWithSquid (cur:rs) cards = playBingoWithSquid rs filteredCards
  where
    updatedCards = map (mark cur) cards
    filteredCards = filter (not . hasWon) updatedCards

-- types --
type BingoCard = [[Int]]
type BingoTracking = [[Bool]]
type TrackedBingoCard = (BingoTracking, BingoCard)

-- PARSER --
file = do 
  firstLine <- calledNumbers <* eol
  eol
  cards <- many1 bingoCard
  eof
  return (firstLine, cards)
-- lines
calledNumbers = sepBy1 number (char ',')
bingoCard     = many (bingoRow <* eol) <* eol
bingoRow      = sepBy1 bingoNumber space
-- trim leading spaces
bingoNumber = many space *> number
-- units
number = read <$> many1 (oneOf "0123456789")
eol    = char '\n'
space  = char ' '

parseInput:: IO ([Int], [BingoCard])
parseInput = Parser.parseFile file "04.txt"

--export
title = "bingo with a squid"
part1 = do
  (nums, cards) <- parseInput
  let (num, tc) = playBingo nums $ map initialTracking cards
  return $ num * score tc

part2 = do
  (nums, cards) <- parseInput
  let (num, tc) = playBingoWithSquid nums $ map initialTracking cards
  return $ num * score tc
