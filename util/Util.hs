module Util where

import System.IO

input :: IO [String]
input = input' []
  where
    input' acc = do
      done <- isEOF
      if done then
        return acc
      else do
        line <- getLine
        input' (line:acc)

split :: Char -> String -> [String]
split c s = split' c s []
  where
    split' c [] acc = reverse acc
    split' c s acc =
      let acc' = takeWhile (/= c) s : acc
      in split' c ((drop 1 . dropWhile (/= c)) s) acc'
