import Numeric.LinearAlgebra

import qualified Data.Text as T

data Hailstone = Hailstone (Vector Double) (Vector Double) deriving (Show, Eq)
data Intersection = Intersection (Vector Double) Double Double deriving Show

toHailstone :: [[Double]] -> Hailstone
toHailstone [[x0,y0,z0], [vx,vy,vz]] = Hailstone (vector [x0, y0, z0]) (vector [vx, vy, vz])
toHailstone got = error $ "wrong format: " ++ show got ++ " is not [[x,y,z],[a,b,c]]"

intersect2D :: Hailstone -> Hailstone -> Intersection
intersect2D (Hailstone p0 v0) (Hailstone p1 v1) = Intersection (vector [psx, psy]) t0 t1
  where
    dx = p0!0 - p1!0
    dy = p0!1 - p1!1

    denom = v0!1 * v1!0 - v0!0 * v1!1
    numer0 = v1!1 * dx - v1!0 * dy
    numer1 = v0!1 * dx - v0!0 * dy
    t0 = numer0 / denom
    t1 = numer1 / denom

    psx = p0!0 + t0 * v0!0
    psy = p0!1 + t0 * v0!1

intersections2D :: [Hailstone] -> [Intersection]
intersections2D [] = []
intersections2D (h:hail) =
  map (intersect2D h) hail ++ intersections2D hail


crossMatrix :: Vector Double -> Matrix Double
crossMatrix vec =
  matrix 3 [ 0,      -vec!2,   vec!1,
             vec!2,   0,      -vec!0,
            -vec!1,   vec!0,   0]

-- Weird voodoo magic. The hailstones' trajectories can be seen as a system of
-- linear equations (https://en.wikipedia.org/wiki/System_of_linear_equations).
-- For some reason it's enough to only take three hailstones as a sample. This gives
-- a linear system with 6 linear equations and 6 unknowns, which has only a single
-- solution.
--
-- The equation system is expressed as Ax = b we solve for x with A'b = x
solveSystem :: [Hailstone] -> Vector Double
solveSystem hails = inv m #> b
  where
    [Hailstone p0 v0, Hailstone p1 v1, Hailstone p2 v2] = take 3 hails 

    b1 = p1 `cross` v1 - p0 `cross` v0
    b2 = p2 `cross` v2 - p0 `cross` v0
    b = vjoin [b1, b2]

    m00 = crossMatrix v0 - crossMatrix v1
    m30 = crossMatrix v0 - crossMatrix v2
    m03 = (-crossMatrix p0) + crossMatrix p1
    m33 = (-crossMatrix p0) + crossMatrix p2

    m = fromBlocks [[m00, m03], [m30, m33]]

main :: IO ()
main = do
  input <- map (T.splitOn " @ ") . T.lines . T.pack <$> getContents
  let parsed = map (toHailstone . map (map (read . T.unpack) . T.splitOn ", ")) input

  let (lo,hi) = (200000000000000.0, 400000000000000.0)
  --let (lo,hi) = (7.0, 27.0)

  let inBounds p = p!0 >= lo && p!0 <= hi && p!1 >= lo && p!1 <= hi
  let inters2D = filter (\(Intersection p t s) -> inBounds p && t > 0 && s > 0) $ intersections2D parsed

  print $ length inters2D

  let ans2 = solveSystem parsed
  let pos = vector [ans2!0, ans2!1, ans2!2]
  let vel = vector [ans2!3, ans2!4, ans2!5]

  putStrLn $ "Position " ++ show pos
  putStrLn $ "Speed " ++ show vel

  print $ round (sumElements pos)
