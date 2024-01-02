import Data.Text (Text)
import Data.Set (Set)
import Data.Map (Map)
import Data.List
import Data.Ord
import Data.Maybe

import qualified Data.Text as T
import qualified Data.Set as Set
import qualified Data.Map as Map

type Coord = (Int, Int, Int)

data Brick = Brick Coord Coord deriving (Show, Eq, Ord)

parseXYZ :: Text -> Coord
parseXYZ str = 
  let res = T.split (== ',') str in
    case res of
      [x,y,z] -> (read $ T.unpack x, read $ T.unpack y, read $ T.unpack z)
      _ -> error "format"

parse :: String -> Brick
parse str =
  let str' = T.pack str in
    let res = T.split (== '~') str' in
      case res of
        [start, end] -> Brick (parseXYZ start) (parseXYZ end)
        _ -> error "format"

minZ :: Brick -> Int
minZ (Brick (_,_,z0) (_,_,z1))= min z0 z1

maxZ :: Brick -> Int
maxZ (Brick (_,_,z0) (_,_,z1)) = max z0 z1

minXY :: Brick -> (Int,Int)
minXY (Brick (x0,y0,_) (x1,y1,_)) = (min x0 x1, min y0 y1)

maxXY :: Brick -> (Int,Int)
maxXY (Brick (x0,y0,_) (x1,y1,_)) = (max x0 x1, max y0 y1)

overlappingXY :: Brick -> Brick -> Bool
overlappingXY b0 b1 =
  overlappingX b0 b1 && overlappingY b0 b1

overlappingX :: Brick -> Brick -> Bool
overlappingX b0 b1
  | x0' < x1 || x1' < x0 = False
  | otherwise = True
  where
    (x0,x0') = (fst $ minXY b0, fst $ maxXY b0)
    (x1,x1') = (fst $ minXY b1, fst $ maxXY b1)

overlappingY :: Brick -> Brick -> Bool
overlappingY b0 b1
  | x0' < x1 || x1' < x0 = False
  | otherwise = True
  where
    (x0,x0') = (snd $ minXY b0, snd $ maxXY b0)
    (x1,x1') = (snd $ minXY b1, snd $ maxXY b1)

settleBricks :: [Brick] -> [Brick]
settleBricks = foldl' settleBrick []

settleBrick :: [Brick] -> Brick -> [Brick]
settleBrick settled brick@(Brick (x0,y0,z0) (x1,y1,z1)) =
  Brick (x0,y0,z0-fallZ) (x1,y1,z1-fallZ) : settled
  where
    below = filter (overlappingXY brick) settled 
    highestBelow
      | null below  = 0
      | otherwise   = maxZ $ maximumBy (comparing maxZ) below
    fallZ = minZ brick - highestBelow - 1

getSupports :: Brick -> [Brick] -> [Brick]
getSupports brick = filter (\b -> maxZ b == minZ brick - 1 && overlappingXY brick b)

makeChain :: [Brick] -> Map Brick (Set Brick) -> Set Brick -> Brick -> Set Brick
makeChain bricks supmap chain brick = foldl' (makeChain bricks supmap) nextChain nextVisit
  where
    nextChain = foldl' addToChain chain directAbove
    directAbove =
      filter (overlappingXY brick) $
      filter ((== maxZ brick + 1) . minZ) bricks

    supportOf brick' = fromJust $ Map.lookup brick' supmap

    nextVisit = Set.toList $ Set.intersection nextChain (Set.fromList directAbove)

    supportsNotPartOfChain chain' brick' = supportOf brick' Set.\\ chain'
    addToChain chain' brick'
      | Set.null $ supportsNotPartOfChain chain' brick' = Set.insert brick' chain'
      | otherwise = chain'

main :: IO ()
main = do
  bricks <- lines <$> getContents
  let bricks' = sortBy (comparing minZ) $ map parse bricks

  let settled = settleBricks bricks'
  let supports = map (\brick -> Set.fromList $ brick `getSupports` settled) settled

  -- part 1
  let notReduntant = foldr Set.union Set.empty $ filter (\s -> Set.size s == 1) supports
  print $ length settled - Set.size notReduntant

  -- part 2
  let supportMap = Map.fromList $ zip settled supports
  let chains = map (\brick -> makeChain settled supportMap (Set.singleton brick) brick) (Set.toList notReduntant)
  let adjusted = map (flip (-) 1 . Set.size) chains
  print $ sum adjusted
