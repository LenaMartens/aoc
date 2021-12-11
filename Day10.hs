module Day10 where

import Data.List
import Data.Stack
import Data.Maybe 
import Text.Parsec
import qualified Parser

-- part 1
parseBrackets:: String -> Stack Char -> Maybe Char 
parseBrackets [] _ = Nothing
parseBrackets (curr:brackets) st
  | curr `notElem` right  = parseBrackets brackets (stackPush st curr) -- a left bracket 
  | closing prev /= curr  = Just curr                                  -- a non-closing right
  | otherwise             = parseBrackets brackets newStack            -- a closing right
    where 
      Just (newStack, prev) = stackPop st

parseIncorrect:: String -> Maybe Char
parseIncorrect (c:br) = parseBrackets br (stackPush stackNew c)
parseIncorrect []     = Nothing

onlyCorrects = filter (isNothing.parseIncorrect)

-- part 2
stackAfterParse:: String -> Stack Char -> Stack Char 
stackAfterParse [] st = st         -- parsing done, return the stack
stackAfterParse (curr:brackets) st 
  | curr `elem` right = stackAfterParse brackets newStack            -- a right bracket
  | otherwise         = stackAfterParse brackets (stackPush st curr) -- a left bracket
    where Just (newStack, _) = stackPop st

toMatchingString:: Stack Char -> String
toMatchingString st = 
  case stackPop st of
       Nothing               -> ""
       Just (newStack, top)  -> closing top:toMatchingString newStack

complete:: String -> String
complete (c:str) = toMatchingString $ stackAfterParse str $ stackPush stackNew c
complete []      = ""

scoreCompletion:: Int -> Char -> Int
scoreCompletion acc c = acc*5+score2 c

median ls = sort ls!!(length ls `div` 2)

-- closing brackets table --
closing '(' = ')'
closing '[' = ']'
closing '{' = '}'
closing '<' = '>'
closing  _  = '^'

right = ")}]>"

-- scores part1 table --
score1 Nothing    = 0
score1 (Just ')') = 3
score1 (Just ']') = 57
score1 (Just '}') = 1197
score1 (Just '>') = 25137
score1 (Just  _ ) = 666
-- scores part2 table --
score2 ')' = 1
score2 ']' = 2
score2 '}' = 3
score2 '>' = 4
score2  _  = 0

-- ======parser======
file = many (many (oneOf "(){}[]<>") <* eol) <* eof
eol = char '\n'

parseInput:: IO [String]
parseInput = Parser.parseFile file "10.txt"

part1 = do
  inp <- parseInput
  print $ sum $ map (score1.parseIncorrect) inp

part2 = do
  inp <- parseInput
  let completed = map complete $ onlyCorrects inp
  print $ median $ map (foldl scoreCompletion 0) completed
