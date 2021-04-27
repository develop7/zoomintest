module Main where

import Lib
import System.Environment (getArgs)
import Text.Read (readEither)

main :: IO ()
main = do
  (amt : _) <- getArgs
  let amt'' = readEither amt
  putStrLn $ case amt'' of
    Right amt' -> show $ calcNISTax amt'
    Left err -> "failed to parse \"" ++ amt ++ "\": " ++ err
