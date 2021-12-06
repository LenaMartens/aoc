module Day4 where

import Data.List
import Text.ParserCombinators.Parsec hiding (space)
import System.Exit
import System.IO

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
playBingoWithSquid nums [card] = playBingo nums [card]
playBingoWithSquid (cur:rs) cards = playBingoWithSquid rs filteredCards
  where
    updatedCards = map (mark cur) cards
    filteredCards = filter (not . hasWon) updatedCards

-- types --
type BingoCard = [[Int]]
type BingoTracking = [[Bool]]
type TrackedBingoCard = (BingoTracking, BingoCard)

-- PARSER --
day4File = do 
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
parseInput = parseFromFile day4File "inp/004.txt" >>= either report return
  where 
    report err = do
      hPutStrLn stderr $ "Error: " ++ show err
      exitFailure

-- solutions
part1 = do
  (nums, cards) <- parseInput
  let (num, tc) = playBingo nums $ map initialTracking cards
  print $ num * score tc

part2 = do
  (nums, cards) <- parseInput
  let (num, tc) = playBingoWithSquid nums $ map initialTracking cards
  print $ num * score tc
