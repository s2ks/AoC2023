import Data.List
import Data.Maybe
import Data.IORef

import Util

import qualified Data.Map as Map

type Record = (String, [Int])
type Memo = Map.Map (String, [Int], Int) Int

countWays :: IORef Memo -> String -> [Int] -> Int -> IO Int
countWays _ [] _ _ = return 0
countWays memo (c:row) (n:groups) current 
  | c == '?' = do 
      a <- lookup ('.':row) (n:groups) current 
      b <- lookup ('#':row) (n:groups) current
      return $ a + b
  | nextn < 0 = return 0
  | current > 0 && n > 0 && c /= '#' = return  0
  | null row && nextn == 0 && null groups = return 1
  | otherwise = lookup row nextgroup nextcurrent
  where
    nextn = if c == '#' then n - 1 else n
    nextcurrent = if c == '#' then current + 1 else 0
    nextgroup = if n == 0 && (not . null) groups then groups else nextn:groups

    memoize key n = do
      memo' <- readIORef memo
      writeIORef memo (Map.insert key n memo')

    lookup r g c = do
      let key = (r,g,c)
      memo' <- readIORef memo

      if Map.member key memo' then do
        return $ fromJust (Map.lookup key memo')
      else do
        res <- countWays memo r g c
        memoize key res
        return res

main :: IO ()
main = do
  lines <- lines <$> getContents

  let rows = map (takeWhile (/= ' ')) lines
  let groups = map (map read . split ',' . drop 1 . dropWhile (/= ' ')) lines :: [[Int]]
  let records = zip rows groups :: [Record]

  -- part 1
  memo <- newIORef Map.empty
  valid <- mapM (\(r, g) -> countWays memo r g 0) records
  print $ sum valid

  -- part 2
  let why = map (\(s, g) ->  ((intercalate "?" . replicate 5) s, (concat . replicate 5) g)) records
  memo <- newIORef Map.empty
  valid2 <- mapM (\(r, g) -> countWays memo r g 0) why
  print $ sum valid2

