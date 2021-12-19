module Day17 (title, part1, part2, exp1, exp2) where

import Data.Maybe (isJust, fromJust)
import Data.Function (on)
import Data.List
import Text.Parsec
import qualified Parser

type Position = (Int, Int)
type Velocity = (Int, Int)
type Area     = ((Int, Int), (Int, Int))

-- part1
step:: (Position, Velocity) -> (Position, Velocity)
step ((x, y), (velX, velY)) = (newPos, (newVelX, newVelY))
  where newPos  = (x+velX, y+velY)
        newVelX
          | velX > 0  = velX - 1
          | velX == 0 = 0
          | otherwise = velX + 1
        newVelY = velY - 1

heightWithTarget:: Area -> Velocity -> Maybe Position
heightWithTarget ((x0, x1), (y0, y1)) initialVel =
  case hasTarget of
    Nothing -> Nothing
    Just _  -> Just $ fst $ maximumBy (compare `on` (snd.fst)) tracking
  where tracking = takeWhile notOvershot $ iterate step ((0, 0), initialVel)
        notOvershot ((x, y), _) = x <= x1*2 && y >= y1*2
        inTarget    ((x, y), _) = (x >= x0 && x<= x1) && (y >= y0 && y <= y1)
        hasTarget = find inTarget tracking


highestHigh:: Area -> Velocity -> Maybe Position
highestHigh target tops = maximumBy (compare `on` snd.fromJust) $ allValidVelocities target tops

cartProd xs ys = [(x,y) | x <- xs, y <- ys]
-- TODO: find better bounds on initial velocity (but it's already fast enough)
tops ((x0, x1), (y0, y1)) = (25 * (x1-x0), 25 * (abs y0 - abs y1))
-- part2
allValidVelocities target (topX, topY)= filter isJust $ map (heightWithTarget target) $ cartProd [-topX..topX] [-topY..topY]

-- ======parser======
file = do
  string "target area: x="
  x0 <- number
  string ".."
  x1 <- number
  string ", y="
  y0 <- number
  string ".."
  y1 <- number
  return ((x0, x1), (y0, y1))
number = read <$> many (oneOf "-0123456789")

testInp = ((20, 30), (-10, -5))

parseInput:: IO ((Int, Int), (Int, Int))
parseInput = Parser.parseFile file "17.txt"

-- public export
title = "probing in style"

part1 = do
  inp <- parseInput
  return $ snd.fromJust $ highestHigh inp $ tops testInp
exp1 = 7750 :: Int

part2 = do
  inp <- parseInput
  return $ length $ allValidVelocities inp $ tops testInp
exp2 = 4120 :: Int
