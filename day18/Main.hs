import qualified Data.Text as T

data Direction = Start|U|D|L|R deriving (Show, Eq, Ord)
type Pos = (Int, Int)
type Plan = (Direction, Int)

type Line = (Pos, Pos)

readHex :: String -> Plan
readHex s =
  let s' = (drop 2 . init . init) s in
    let d' = (last . init) s in
    (readDirection2 [d'], read ("0x" ++ s'))

readDirection :: String -> Direction
readDirection "U" = U
readDirection "D" = D
readDirection "L" = L 
readDirection "R" = R
readDirection _ = error ""

readDirection2 :: String -> Direction
readDirection2 "0" = R
readDirection2 "1" = D
readDirection2 "2" = L
readDirection2 "3" = U
readDirection2 _ = error ""

parse :: [String] -> Plan
parse [d, s, _] = (readDirection d, read s)

parse2 :: [String] -> Plan
parse2 [_,_,c] = readHex c

nextPos :: Pos -> Int -> Direction -> Pos
nextPos (x,y) steps d
  | d == L = (x-steps,y)
  | d == R = (x+steps,y)
  | d == U = (x,y-steps)
  | d == D = (x,y+steps)
  | otherwise = error ""

execPlan :: Pos -> Plan -> Line
execPlan pos (dir, steps) = (pos, nextPos pos steps dir)

mkOutline :: Pos -> [Plan] -> [Line]
mkOutline _ [] = []
mkOutline cur (plan:plans) =
  let line = execPlan cur plan in
    let (_,cur') = line in
      line: mkOutline cur' plans

bounds :: [Line] -> (Pos, Pos)
bounds lines = ((minx,miny), (maxx,maxy))
  where
    coords = concatMap (\line -> [fst line, snd line]) lines
    xs = map fst coords
    ys = map snd coords
    minx = minimum xs
    maxx = maximum xs
    miny = minimum ys
    maxy = maximum ys

fill :: [Line] -> Int
fill lines = sum (map (\((x0,y0), (x1,y1)) -> x0*y1 - x1*y0) lines)

main :: IO ()
main = do
  instructions <- map (map T.unpack . T.split (== ' ')) . T.lines . T.pack <$> getContents 
  let plans = map parse instructions

  let outlineSum = sum (map snd plans)
  let outlines = mkOutline (0,0) plans
  let ans = 1 + (fill outlines + outlineSum) `div` 2
  print ans

  let plans2 = map parse2 instructions
  let outlineSum2 = sum (map snd plans2)
  let outlines2 = mkOutline (0,0) plans2
  let ans2 = 1 + (fill outlines2 + outlineSum2) `div` 2

  print ans2
