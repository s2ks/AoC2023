import Data.Graph.Inductive
import Data.Graph.Inductive.Dot

import System.FilePath
import System.IO
import System.Process

import Data.Set (Set)
import Data.Map (Map, (!))

import qualified Data.Text as T
import qualified Data.Set as Set
import qualified Data.Map as Map

getEdges :: Map String [String] -> Set String -> String -> [LEdge Int]
getEdges kvMap nodes' node = map (src,,1) dests
  where
    src = Set.findIndex node nodes'
    idx n' = Set.findIndex n' nodes'
    dests = map idx $ kvMap ! node

graphToDot :: (Graph gr, Show a, Show b) => gr a b -> IO ()
graphToDot g = do
  let tmp = "./"
  (dot, h) <- openTempFile tmp "graph.dot"
  let png = dot -<.> "svg"
  hPutStr h (showDot (fglToDot g))
  hClose h

  callProcess "dot" ["-Tsvg", "-o", png, dot]

main :: IO ()
main = do
  connections <- map (map (map T.unpack . T.splitOn " ") . T.splitOn ": ") . T.lines . T.pack <$> getContents
  let kv = map (\conn -> ((head . head) conn, last conn)) connections

  let kvMap = Map.fromList kv
  let nodes' = Set.fromList $ concatMap concat connections
  let edges' = concatMap (getEdges kvMap nodes') (nodes' `Set.intersection` Map.keysSet kvMap)

  let graph = mkGraph (zip [0..] (Set.toList nodes')) edges' :: Gr String Int

  -- Ghetto solution, but... Convert the graph to an svg, find the point
  -- in the middle of the graph where only three lines connect the
  -- two parts. Open the svg in vim and delete all edges with
  -- :g/edge/normal dat
  -- Open the edited svg in inkscape, select all edges on the
  -- left side of the point you found earlier, note the number
  -- of selected items and do the same for the right side.
  -- Multiply the two numbers together... And done :)
  graphToDot graph
  
  -- Maybe I'll come back to this someday to implement a proper solution
  -- using karger or stoer-wagner
  -- 
  -- probably not.

  -- *Press the big red button*
