import Data.List
import Data.Maybe

import qualified Data.Map as Map

moveBalls :: String -> String -> (String, String)
moveBalls [] [] = ([], [])
moveBalls (s:from) (d:to)
  | s == 'O' && d == '.' = ('.':nxtfrom, 'O':nxtto)
  | otherwise = (s:nxtfrom, d:nxtto)
    where
      (nxtfrom, nxtto) = moveBalls from to

-- from a current row with the given previous rows, keep moving all balls
-- north until there are no more previous rows (we hit the top)
moveNorth :: [String] -> [String] -> [String]
moveNorth [] current = current
moveNorth (prev:prevs) (cur:nexts) =
  let (cur', prev') = moveBalls cur prev in
    moveNorth prevs (prev':cur':nexts)

moveNorthRecurse :: [String] -> [String] -> [String]
moveNorthRecurse prevs [] = prevs
moveNorthRecurse prevs nexts = moveNorthRecurse prevs' nexts'
  where
    moved = moveNorth prevs nexts
    prevs' = reverse $ take (length prevs + 1) moved
    nexts' = drop (length prevs + 1) moved

-- Keep track of which states we've seen and at which depth in the count
type Memo = Map.Map [String] Int

cycleDish :: Memo -> [String] -> Int -> (Int, Int, [String])
cycleDish memo rows count
  | count > 1 =
    let result = east rows in
      if Map.member result memo then
        let lastSeen = fromJust $ Map.lookup result memo in
          (lastSeen, count, result)
      else
        let memo' = Map.insert result count memo in
            cycleDish memo' result (count - 1)
  | otherwise = (count, count, east rows)
  where
    north rows = reverse $ moveNorthRecurse [] rows
    west rows = (transpose . reverse) $ moveNorthRecurse [] (transpose (north rows))
    south rows = moveNorthRecurse [] (reverse (west rows))
    east rows = transpose $ moveNorthRecurse [] ((reverse . transpose) (south rows))

main :: IO ()
main = do
  lines <- lines <$> getContents

  -- part 1
  let lines' = moveNorthRecurse [] lines
  let ans = zipWith (\line i -> i * length (filter (== 'O') line)) lines' [1..]
  print $ sum ans

  -- part 2
  let (last, cur, state) = cycleDish Map.empty lines 1000000000
  let loopLength = last - cur
  let remain = cur `rem` loopLength
  let (last', cur', state') = cycleDish Map.empty state (remain -1)
  let ans2 = zipWith (\line i -> i * length (filter (== 'O') line)) (reverse state') [1..]
  print $ sum ans2

