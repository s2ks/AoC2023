{-# LANGUAGE TupleSections #-}

import Data.IORef

import qualified Data.Map as Map
import qualified Data.Set as Set

type Pos = (Int, Int)
data Direction = U | D | L | R deriving (Show, Eq, Ord)

coords :: [[Pos]]
coords = map (\y -> map (, y) [0..]) [0..]

nextDirections :: Direction -> Char -> [Direction]
nextDirections U '/' = [R]
nextDirections U '\\' = [L]
nextDirections U '-' = [L,R]

nextDirections D '/' = [L]
nextDirections D '\\' = [R]
nextDirections D '-' = [L,R]

nextDirections L '/' = [D]
nextDirections L '\\' = [U]
nextDirections L '|' = [U,D]

nextDirections R '/' = [U]
nextDirections R '\\' = [D]
nextDirections R '|' = [U,D]

nextDirections d _ = [d]

nextPos :: Direction -> Pos -> Pos
nextPos U (x, y) = (x, y-1)
nextPos D (x, y) = (x, y+1)
nextPos L (x, y) = (x-1, y)
nextPos R (x, y) = (x+1, y)

symbol :: Pos -> Map.Map Pos Char -> Char
symbol p kv = kv Map.! p

nextCell :: Direction -> Pos -> Map.Map Pos Char -> Maybe Pos
nextCell d p kv =
  let p' = nextPos d p in
    if Map.member p' kv then
      Just p'
    else
      Nothing


search :: Direction -> Pos -> Map.Map Pos Char -> IORef (Set.Set (Direction, Pos)) -> IO [(Direction,Pos)]
search d p kv rseen = do 
  seen <- readIORef rseen
  if Set.member (d, p) seen
    then return []
    else do
      addSeen
      rs <- concat <$> mapM (\(p', d') -> search d' p' kv rseen) psds'
      return $ (d,p): rs
  where
    ds' = nextDirections d (symbol p kv)
    ps' = map (`nextPos` p) ds'
    psds' = filter (\(p',_) -> Map.member p' kv) $ zip ps' ds'
    addSeen = readIORef rseen >>= writeIORef rseen . Set.insert (d, p)


allEdgeTiles :: Map.Map Pos Char -> [(Direction, [Pos])]
allEdgeTiles kv = [leftEdge, topEdge, rightEdge, bottomEdge]
  where
    (xmin, ymin) = fst $ Map.findMin kv
    (xmax, ymax) = fst $ Map.findMax kv
    leftEdge = (R, map (xmin,) [ymin..ymax])
    rightEdge = (L, map (xmax,) [ymin..ymax])
    topEdge = (D, map (,ymin) [xmin..xmax])
    bottomEdge = (U, map (,ymax) [xmin..xmax])


tryAllEdges :: Direction -> [Pos] -> Map.Map Pos Char -> IO [Int]
tryAllEdges _ [] _ = return []
tryAllEdges d (edge:edges) kv = do
  seen <- newIORef Set.empty
  visited <- search d edge kv seen

  let unique = foldr (\(_, pos) s -> Set.insert pos s) Set.empty visited
  let len = length unique

  rs <- tryAllEdges d edges kv
  return (len:rs)

main :: IO ()
main = do
  lines <- lines <$> getContents

  let grid' = concat $ zipWith zip lines coords
  let grid = foldr (\(c, coord) kvMap -> Map.insert coord c kvMap) Map.empty grid'

  -- part 1
  seen <- newIORef Set.empty
  visited <- search R (0, 0) grid seen
  let unique = foldr (\(_, pos) s -> Set.insert pos s) Set.empty visited
  print $ length unique

  -- part 2
  lens <- mapM (\(d, edges) -> tryAllEdges d edges grid) $ allEdgeTiles grid
  let maxs = map maximum lens
  let max = maximum maxs
  print max
