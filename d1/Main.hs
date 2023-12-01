module Main where

import System.IO
import Data.Char

import Data.Text (pack)
import Data.Text.Internal.Search
import Data.List

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
      

-- Pt1
extractNum :: String -> Int
extractNum s = let digits = filter isDigit s in
    read [head digits, last digits]

-- pt2
_EXTRA_NUMS = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

toInt :: String -> Int
toInt "one"   = 1
toInt "two"   = 2
toInt "three" = 3
toInt "four"  = 4
toInt "five"  = 5
toInt "six"   = 6
toInt "seven" = 7
toInt "eight" = 8
toInt "nine"  = 9
toInt s = read s :: Int


isNum :: String -> Bool
isNum "one"   = True
isNum "two"   = True
isNum "three" = True
isNum "four"  = True
isNum "five"  = True
isNum "six"   = True
isNum "seven" = True
isNum "eight" = True
isNum "nine"  = True
isNum _ = False


-- >>> takeNum "8thgiethgie1thgie" []
-- 8

takeNum :: String -> String -> Int
takeNum (c:cs) [] = if isDigit c then toInt [c] else takeNum cs [c]
takeNum (c:cs) acc = let chk = reverse (c:acc) in if isNum chk then toInt chk else takeNum cs (c:acc)
takeNum [] acc = error ("acc: " <> show acc)

-- >>> concatMap (\w -> w `elemIndices` map ((take . length) w) (tails "hello")) ["he", "o"]
-- [0,4]
--
-- >>> tails "hello"
-- ["hello","ello","llo","lo","o",""]
extractNum2 s =
  let words = filter (`isSubsequenceOf` s) _EXTRA_NUMS
      wordsIndices = sort $ concatMap (\w -> w `elemIndices` map ((take . length) w) (tails s)) words
      digits = filter isDigit s
      maybeDigits = case digits of
        [] -> Nothing
        _ -> Just (head (head digits `elemIndices` s), last (last digits `elemIndices` s))
      maybeWords =
        case wordsIndices of
          [] -> Nothing
          _ -> Just (head wordsIndices, last wordsIndices)
      allIndices =
        case (maybeWords, maybeDigits) of
          (Nothing, Nothing) -> error ""
          (Just (fw, lw), Just (fd, ld)) -> sort [fw, lw, fd, ld]
          (Nothing, Just (fd, ld)) -> [fd, ld]
          (Just (fw, lw), Nothing) -> [fw, lw]
      (allFirst, allLast) = (head allIndices, last allIndices)
  in
    takeNum (drop allFirst s) [] * 10 + takeNum (drop allLast s) []
    
main :: IO ()
main = do
  lines <- input

  -- pt1
  let ans1 = foldr (\s acc -> extractNum s + acc) 0 lines
  print $ "Part 1: " <> show ans1

  -- pt2
  let ans2 = foldr (\s acc -> extractNum2 s + acc) 0 lines
  print $ "Part 2: " <> show ans2 
