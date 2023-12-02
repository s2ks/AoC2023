module Main where

import Util

isValid :: [(Int, Int, Int)] -> Bool
isValid xs = let invalid = filter (\(r, g, b) -> r > 12 || g > 13 || b > 14) xs
  in null invalid

toRGB :: String -> (Int, Int, Int)
toRGB s = 
  let parseMe = map (drop 1) (split ',' s)
  in
    toRGB' parseMe (0, 0, 0)
  where
    toRGB' [] acc = acc
    toRGB' (cube:xs) acc@(r, g, b) =
      let ncol = split ' ' cube
          (n, col) = (head ncol, last ncol)
      in
        toRGB' xs $ case col of
          "red" -> (read n, g, b)
          "green" -> (r, read n, b)
          "blue" -> (r, g, read n)


counts :: String -> [(Int, Int, Int)]
counts s = 
  let game = (drop 1 . dropWhile (/= ':')) s
      sets = split ';' game
  in
    map toRGB sets

gameID :: String -> Int
gameID s = let id = (drop (length "Game ") . takeWhile (/= ':')) s in read id

power :: (Int, Int, Int) -> Int
power (r, g, b) = r * b * g

maxRGB :: [(Int, Int, Int)] -> (Int, Int, Int)
maxRGB = foldr (\(r, g, b) (r', g', b') -> (max r r', max g g', max b b')) (0, 0, 0)

main :: IO ()
main = do
  lines <- input

  -- part 1
  let games = zip (map gameID lines) (map counts lines)
  let validGames = filter (\(_, game) -> isValid game ) games
  let ans1 = foldr (\(id, _) acc -> acc + id) 0 validGames

  print ans1

  -- part 2
  let games2 = map counts lines
  let ans2 =  foldr ((\rgb acc -> acc + power rgb) . maxRGB) 0 games2

  print ans2

