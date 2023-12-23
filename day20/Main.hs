{-# LANGUAGE OverloadedStrings #-}

import Data.Maybe
import Data.Map (Map)

import qualified Data.Text as T
import qualified Data.Map as Map

data Pulse = High | Low | None deriving (Show, Eq, Ord)
data Module =
  FlipFlop Bool [String]
    | Conjunction (Map String Pulse) [String]
    | Broadcaster [String] deriving (Show,Eq,Ord)


instance ModuleClass Module where
  links (FlipFlop _ ls) = ls
  links (Conjunction _ ls) = ls
  links (Broadcaster ls) = ls

  sendPulse (FlipFlop on ms) _ Low =
    let m' = FlipFlop (not on) ms in
      if on then (Low, m') else (High, m')
  sendPulse (Conjunction mem ms) from pulse =
    let mem' = Map.insert from pulse mem in
      let m' = Conjunction mem' ms in
        let b = all ((== High) . snd) $ Map.toList mem' in
          if b then (Low, m') else (High, m')
  sendPulse m@(Broadcaster _) _ pulse = (pulse, m) 
  sendPulse m _ _ = (None, m)

class ModuleClass a where
  links :: a -> [String]
  sendPulse :: a -> String -> Pulse -> (Pulse,a)

parseModule :: String -> [String] -> (String, Module)
parseModule ('%':label) linked = (label, FlipFlop False linked)
parseModule ('&':label) linked = (label, Conjunction Map.empty linked)
parseModule label@"broadcaster" linked = (label, Broadcaster linked)
parseModule label _ = error $ "unknown module " <> label

initialState :: Map String Module -> Map String Module
initialState state = Map.foldrWithKey (\k m state' -> update state' k $ linkedMods state' $ links m) state state
  where
    linkedMods state' = map (\l -> (l, fromJust $ Map.lookup l state')) . filter (`Map.member` state')
    update state' _ [] = state'
    update state' k ((name, Conjunction mem ls):ms) =
      let conj' = Conjunction (Map.insert k Low mem) ls in
        let state'' = Map.insert name conj' state'
            in update state'' k ms
    update state' k (_:ms) = update state' k ms

simulate :: [(String,Pulse)] -> Map String Module -> (Int,Int,Map String Module)
simulate front state
  | null front = (1,0,state)
  | otherwise = do
    let (lows,highs,state') = simulate nextFront' nextState in
      (lows+frontLows, highs+frontHighs, state')
  where
    frontLows = sum $ map (\(s,_) -> adjacent s) $ filter (\(_, p) -> p == Low) front
    frontHighs = sum $ map (\(s,_) -> adjacent s) $ filter (\(_, p) -> p == High) front
    adjacent s = let m = toModule s in length $ links m
    nextState = foldr (\(s, (_, m)) state' -> Map.insert s m state') state nextFront
    toModule s = if Map.member s state then fromJust $ Map.lookup s state else error s
    nextFront =
      let mods = map (uncurry expand) front in
        concat mods

    nextFront' = map (\(s, (p, _)) -> (s,p)) nextFront
    expand s p =
      let m = toModule s in
        let links' = filter (`Map.member` state) $ links m in
          filter (\(_,(p',_)) -> p' /= None) $ map (\s' -> let m' = toModule s' in (s', sendPulse m' s p)) links'

-- Find after how many loops we send a high pulse to "lv", for my input, lv connects to rx
simulate2 :: [(String,Pulse)] -> Map String Module -> Int -> Map String Int -> (Map String Int, Map String Module)
simulate2 front state loop acc
  | null front = (acc, state)
  | otherwise =
      let allAdjacents = concatMap (\(s,p) -> zip3 (repeat s) (repeat p) (links (toModule s))) front in
        let st = filter (\(s,p,dest) -> dest == "lv" && p == High && s `Map.notMember` acc) allAdjacents in
          let acc' = foldr (\(src,_,_) acc'' -> Map.insert src (loop+1) acc'') acc st in
            simulate2 nextFront' nextState loop acc'
  where
    nextState = foldr (\(s, (_, m)) state' -> Map.insert s m state') state nextFront
    toModule s = if Map.member s state then fromJust $ Map.lookup s state else error s
    nextFront =
      let mods = map (uncurry expand) front in
        concat mods

    nextFront' = map (\(s, (p, _)) -> (s,p)) nextFront
    expand s p =
      let m = toModule s in
        let links' = filter (`Map.member` state) $ links m in
          filter (\(_,(p',_)) -> p' /= None) $ map (\s' -> let m' = toModule s' in (s', sendPulse m' s p)) links'

simLoop :: Int -> (Int,Int) -> Map String Module -> Int
simLoop loops acc@(alows, ahighs) state
  | loops > 0 = do
    let (lows,highs,state') = simulate [("broadcaster", Low)] state in
      simLoop (loops - 1) (lows+alows, highs+ahighs) state'
  | otherwise = uncurry (*) acc


-- 4 modules connect to lv, which solely connects to rx, so after we have those
-- 4 modules and the number of loops after which we send a high pulse to it we can stop.
simLoop2 :: Int -> Map String Module -> Map String Int -> Map String Int
simLoop2 loops state acc = do
  let (acc', state') = simulate2 [("broadcaster", Low)] state loops acc in
    if Map.size acc' == 4
      then acc'
      else simLoop2 (loops + 1) state' acc'

main :: IO ()
main = do
  modules <- map (T.splitOn " -> ") . T.lines . T.pack <$> getContents

  let modules' = map (\m -> (head m, T.splitOn ", " $ last m)) modules
  let moduleMap = Map.fromList $ map (\(lab, link) -> parseModule (T.unpack lab) (map T.unpack link)) modules'

  -- part 1
  let state = initialState moduleMap
  let ans = simLoop 1000 (0,0) state
  print ans

  -- part 2
  let res = simLoop2 0 state Map.empty
  let vs = map snd $ Map.toList res
  let ans2 = foldr lcm 1 vs
  print ans2
