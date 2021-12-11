module Parser (parseFile) where

import Text.Parsec.String (parseFromFile)
import System.Exit
import System.IO

parseFile parser file = parseFromFile parser ("inp/"++file) >>= either report return
  where 
    report err = do
      hPutStrLn stderr $ "Parser Error: " ++ show err
      exitFailure
