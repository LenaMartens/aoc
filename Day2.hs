module Day2 (part1, part2) where

import Data.List.Split

data Direction = Forward | Down | Up deriving (Show)

updateAim:: (Int, Int, Int) -> (Direction, Int) -> (Int, Int, Int)
updateAim (hor, vert, aim) (Forward, amount) = (hor+amount, vert+(aim*amount), aim) 
updateAim (hor, vert, aim) (Down, amount) = (hor, vert, aim+amount) 
updateAim (hor, vert, aim) (Up, amount) = (hor, vert, aim-amount) 

updatePos:: (Int, Int) -> (Direction, Int) -> (Int, Int)
updatePos (hor, vert) (Forward, amount) = (hor+amount, vert) 
updatePos (hor, vert) (Down, amount) = (hor, vert+amount) 
updatePos (hor, vert) (Up, amount) = (hor, vert-amount) 

parseText:: String -> (Direction, Int)
parseText inp = (dir, amount)
  where [dirText, amountText] = splitOn " " inp
        amount = read amountText
        dir = case dirText of
                'f':_ -> Forward
                'd':_ -> Down
                'u':_ -> Up

readInput:: IO [(Direction, Int)]
readInput = do
  contents <- readFile "inp/02.txt"
  return $ map parseText $ lines contents

part1 = do
  input <- readInput
  let (x, y) = foldl updatePos (0, 0) input
  print (x*y)
part2 = do
  input <- readInput
  let (x, y, _) = foldl updateAim (0, 0, 0) input
  print (x*y)
