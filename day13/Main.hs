import Data.List

import Util

isPerfectMirror :: [String] -> [String] -> Bool
isPerfectMirror [] _ = True
isPerfectMirror _ [] = True
isPerfectMirror (x:prev) (y:next) 
  | x == y = isPerfectMirror prev next
  | otherwise = False

mirrorIdx :: [String] -> [String] -> Int -> Int
mirrorIdx _ [] _ = 0
mirrorIdx _ [_] _ = 0
mirrorIdx prev (x:y:xs) cur
  | x == y && isPerfectMirror (x:prev) (y:xs) = cur
  | otherwise = mirrorIdx (x:prev) (y:xs) (cur+1)

allReflectionLines :: [String] -> [String] -> [Int] -> Int -> [Int]
allReflectionLines _ [] acc _ = acc
allReflectionLines _ [_] acc _ = acc
allReflectionLines prev (x:y:xs) acc cur
  | x == y && isPerfectMirror (x:prev) (y:xs) = allReflectionLines (x:prev) (y:xs) (cur:acc) (cur+1)
  | otherwise = allReflectionLines (x:prev) (y:xs) acc (cur+1)

allOfTheseArePossible :: [String] -> [String] -> [[String]]
allOfTheseArePossible _  []  = []
allOfTheseArePossible prev (row:rows) = 
  fixed ++ allOfTheseArePossible (row:prev) rows
  where
    fixed = map (\perm -> prev' ++ (perm:rows)) (everyRowPossible [] row [])
    prev' = reverse prev

    everyRowPossible :: String -> String -> [String] -> [String]
    everyRowPossible _ [] acc = reverse acc
    everyRowPossible prevcs (c:row') acc
      | c == '.' = everyRowPossible (c:prevcs) row' ((reverse prevcs ++ '#':row') : acc)
      | c == '#' = everyRowPossible (c:prevcs) row' ((reverse prevcs ++ '.':row') : acc)

valid :: [String] -> Int -> [Int]
valid m old =
  let rows = map (*100) $ allReflectionLines [] m [] 1 in
    let cols = allReflectionLines [] (transpose m) [] 1 in
      let rows' = filter (/= old) rows in
        let cols' = filter (/= old) cols in
          rows' ++ cols'

main :: IO ()
main = do
  lines <- lines <$> getContents

  let maps = split "" lines

  -- part 1
  let ansrows = map (\m -> 100 * mirrorIdx [] m 1) maps
  let anscols = map (\m -> mirrorIdx[] (transpose m) 1) maps
  let ans = sum anscols + sum ansrows
  print ans


  -- part 2
  let perMap = zipWith (+) anscols ansrows
  let theseArePossible = map (allOfTheseArePossible []) maps
  let zipped = zip theseArePossible perMap
  let newReflections = map (\(ms,old) -> head . head $ filter (not . null) $ map (`valid` old) ms) zipped
  print $ sum newReflections
