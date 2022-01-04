{-# LANGUAGE ParallelListComp #-}
module Day20 (title, part1, part2, exp1, exp2) where

import Debug.Trace
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Text.Parsec
import qualified Parser
import Data.Maybe (fromJust, fromMaybe)

type Pixel = Char

toMap:: [[Pixel]] -> Map (Int, Int) Pixel
toMap grid = M.fromList $ concat [[((y, x), a)
                                   | a<-row    | x<-[0..]]
                                   | row<-grid | y<-[0..]]

neighbs:: Map (Int, Int) Pixel -> (Int, Int) -> Pixel -> [Pixel]
neighbs mp (x, y) infinitePixel = [pixelLookup (i, j)
                                  | i <- [x - 1, x, x + 1],
                                    j <- [y - 1, y, y + 1]]
  where pixelLookup coord = fromMaybe infinitePixel (M.lookup coord mp)

toIndex:: [Pixel] -> Int
toIndex pxs = unDigits $ map toBin pxs

enhance:: Map (Int, Int) Pixel -> [Pixel] -> Pixel -> Map (Int, Int) Pixel
enhance mp alg infinitePixel = M.fromList newGrid
  where newPixel coord = alg!!toIndex (neighbs mp coord infinitePixel)
        newGrid = concat [[((x, y), newPixel (x, y))| x<-xBound]| y<-yBound]
        (xBound, yBound) = bounds mp 0

bounds:: (Map (Int, Int) b -> Int -> ([Int], [Int]))
bounds mp border = ([minX..maxX], [minY..maxY])
  where
  (xs, ys) = unzip $ map fst $ M.toList mp
  maxX =  maximum xs+border
  minX =  minimum xs-border
  maxY =  maximum ys+border
  minY =  minimum ys-border

addBorder:: (Map (Int, Int) Char -> Map (Int, Int) Char)
addBorder mp = foldl (\m k -> M.insertWith (\new old->old) k '.' m) mp $ [(x, y)| x <- xs, y <- ys]
  where (xs, ys) = bounds mp 50

toBin:: Pixel -> Int
toBin '.' = 0
toBin '#' = 1
toBin  x  = -1

unDigits = foldl (\ a b -> a * 2 + b) 0

enhanceN n alg mp = iterate (\(m, i) -> (enhance m alg i, cyc alg i)) (mp, '.')!!n

cyc :: [Pixel] -> Pixel -> Pixel
cyc alg '.' = head alg
cyc alg '#' = last alg
cyc alg  _  = error "uh"

inRegion (x, y) grid = x >= 0 && x < length grid && y >= 0 && y < length (head grid)

printGrid mp = unlines [[fromMaybe ' ' (M.lookup (y, x) mp) | x <- xs] | y <- ys]
  where (xs, ys) = bounds mp 0

deb = putStr . printGrid
-- ======parser======
file = do
  algorithm <- many pixel <* eol
  eol
  image <- many pixels
  eof
  return (algorithm, image)
pixels = many pixel <* eol
pixel  = oneOf ".#"
eol = char '\n'

parseInput:: IO ([Pixel], [[Pixel]])
parseInput = Parser.parseFile file "20.txt"

-- public export
title = "anti-aliasing an infinite border"

part1 = do
  (alg, grid) <- parseInput
  let start = addBorder $ toMap grid
  return $ M.size $ M.filter (=='#') $ fst $ enhanceN 2 alg start
exp1 = 5563 :: Int

part2 = do
  (alg, grid) <- parseInput
  let start = addBorder $ toMap grid
  return $ M.size $ M.filter (=='#') $ fst $ enhanceN 50 alg start
exp2 = 19743 :: Int
