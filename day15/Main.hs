import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Data.Char
import Data.List

-- Holiday ASCII String Helper
hash :: String -> Int
hash = foldl' (\a c -> (ord c + a) * 17 `rem` 256) 0

type Box = (String, Int)

operate :: MV.IOVector [Box] -> [(String, String)] -> IO ()
operate boxes [] = return ()
operate boxes (seq:seqs) = do
  let (label, op) = seq
  let i = hash label

  exec label op i
  operate boxes seqs
  
  where
    exec :: String -> String -> Int -> IO ()
    exec lab ['=', n'] i = do
      let n = read [n']
      cur <- MV.read boxes i
      let (tail', head') = span (\a -> fst a /= lab) cur

      if null head' then
        MV.write boxes i ((lab, n):cur)
      else
        MV.write boxes i (tail' ++ (lab, n):drop 1 head')

    exec lab ['-'] i = do
      cur <- MV.read boxes i
      let cur' = filter (\e -> fst e /= lab) cur
      MV.write boxes i cur'
  
main :: IO ()
main = do
  sequence <- map T.unpack . T.split (== ','). T.strip . T.pack <$> getContents

  -- part 1
  let vals = map hash sequence
  print $ sum vals

  -- part 2
  let ops = map (span isAlpha) sequence
  boxes <- MV.replicate 256 []
  operate boxes ops

  freezed <- V.unsafeFreeze boxes
  let boxes' = map reverse $ V.toList freezed
  let ans2 = zipWith (\slots box -> zipWith (\(_, foc) n -> box * n * foc) slots [1..]) boxes' [1..]
  let ans2' = map sum ans2

  print $ sum ans2'
