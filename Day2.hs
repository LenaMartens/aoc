module Day2 (title, part1, part2) where

import Data.List.Split

data Direction = Forward | Down | Up | FaultyInstruction deriving (Show)

updateAim:: (Int, Int, Int) -> (Direction, Int) -> (Int, Int, Int)
updateAim (hor, vert, aim) (Forward, amount) = (hor+amount, vert+(aim*amount), aim) 
updateAim (hor, vert, aim) (Down, amount)    = (hor, vert, aim+amount) 
updateAim (hor, vert, aim) (Up, amount)      = (hor, vert, aim-amount) 
updateAim _ (FaultyInstruction, _)           = (0, 0, 0) 

updatePos:: (Int, Int) -> (Direction, Int) -> (Int, Int)
updatePos (hor, vert) (Forward, amount) = (hor+amount, vert) 
updatePos (hor, vert) (Down, amount)    = (hor, vert+amount) 
updatePos (hor, vert) (Up, amount)      = (hor, vert-amount) 
updatePos _ (FaultyInstruction, _)      = (0, 0) 

parseText:: String -> (Direction, Int)
parseText inp = (dir, amount)
  where [dirText, amountText] = splitOn " " inp
        amount = read amountText
        dir = case dirText of
                'f':_ -> Forward
                'd':_ -> Down
                'u':_ -> Up
                _     -> FaultyInstruction

readInput:: IO [(Direction, Int)]
readInput = do
  contents <- readFile "inp/02.txt"
  return $ map parseText $ lines contents

--export
title = "diving instructions"
part1 = do
  input <- readInput
  let (x, y) = foldl updatePos (0, 0) input
  return (x*y)
part2 = do
  input <- readInput
  let (x, y, _) = foldl updateAim (0, 0, 0) input
  return (x*y)
