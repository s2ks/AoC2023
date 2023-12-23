{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Text (Text)
import Data.Map (Map)
import Data.Maybe

import qualified Data.Text as T
import qualified Data.Map as Map

type Label = String
type Workflow = [Filter]
type Range = (Int, Int)

data Var = X | M | A | S deriving Show
data Condition = Condition Var Ordering Int deriving Show
data Filter = Filter Condition Action | Fallback Action deriving Show

data Action = Goto Label | Accept | Reject deriving Show

data Item = Item {
  x :: Int,
  m :: Int,
  a :: Int,
  s :: Int
} deriving Show

data RangeItem = RangeItem {
  xx :: Range,
  mm :: Range,
  aa :: Range,
  ss :: Range
} deriving Show 


parseVar :: Text -> Text -> (Var, Int)
parseVar var v
  | var == "x" = (X, read $ T.unpack v)
  | var == "m" = (M, read $ T.unpack v)
  | var == "a" = (A, read $ T.unpack v)
  | var == "s" = (S, read $ T.unpack v)
  | otherwise = error $ "unk var " <> show var


parseAction :: Text -> Action
parseAction txt
  | txt == "A" = Accept
  | txt == "R" = Reject
  | otherwise = Goto $ T.unpack txt

parseFilter :: Text -> Filter
parseFilter txt
  | T.elem '>' txt =
    let (cond, label) = T.break (== ':') txt in
      let (var, v) = T.break (== '>') cond in
        let (var', val') = parseVar var (T.drop 1 v) in
          let cond' = Condition var' GT val' in
            Filter cond' (parseAction $ T.drop 1 label)
  | T.elem '<' txt =
    let (cond, label) = T.break (== ':') txt in
      let (var, v) = T.break (== '<') cond in
        let (var', val') = parseVar var (T.drop 1 v) in
          let cond' = Condition var' LT val' in
            Filter cond' (parseAction $ T.drop 1 label)
  | otherwise = Fallback $ parseAction txt

parseWorkflow :: Text -> (Label, Workflow)
parseWorkflow txt =
  let (label, txt') = T.breakOn "{" txt in
    let conds = T.splitOn "," (T.drop 1 $ T.init txt') in
      let filts = map parseFilter conds in
        (T.unpack label, filts)

parseItem :: Text -> Item
parseItem txt =
  let txt' = (T.drop 1 . T.dropEnd 1) txt in
    let vars = T.splitOn "," txt' in
      let [(_,x'),(_,m'),(_,a'),(_,s')] = map (\v -> let [var, val] = T.splitOn "=" v in parseVar var val) vars in
        Item {x=x',m=m',a=a',s=s'}


conditionHolds :: Item -> Condition -> Bool
conditionHolds Item{..} cond = 
  case cond of
    Condition X ord v -> holds x ord v
    Condition M ord v -> holds m ord v
    Condition A ord v -> holds a ord v
    Condition S ord v -> holds s ord v
  where
    holds i GT j = i > j
    holds i LT j = i < j

processAction :: Item -> Workflow -> Action
processAction item (filt:filts) =
  case filt of
    Fallback act -> act
    Filter cond act ->
      if conditionHolds item cond
        then act
        else processAction item filts

proccessItem :: Item -> String -> Map String Workflow -> Maybe Item
proccessItem item label workflows =
  let workflow = fromJust $ Map.lookup label workflows in
    let action = processAction item workflow in
      case action of
        Accept -> Just item
        Reject -> Nothing
        Goto label -> proccessItem item label workflows

filterParts :: [Item] -> Map String Workflow -> [Item]
filterParts [] _ = []
filterParts (item:items) workflows =
  let result = proccessItem item "in" workflows in
    case result of
      Nothing -> filterParts items workflows
      Just _ -> item:filterParts items workflows
  

branchOnCondition :: RangeItem -> Condition -> (RangeItem, RangeItem)
branchOnCondition item@RangeItem{..} (Condition var ord val) = branch var ord val
  where
    branch X GT v = let (lo,hi) = xx in (item {xx = (lo, v)}, item {xx = (v+1, hi)})
    branch X LT v = let (lo,hi) = xx in (item {xx = (v, hi)}, item {xx = (lo, v-1)})

    branch M GT v = let (lo,hi) = mm in (item {mm = (lo, v)}, item {mm = (v+1, hi)})
    branch M LT v = let (lo,hi) = mm in (item {mm = (v, hi)}, item {mm = (lo, v-1)})

    branch A GT v = let (lo,hi) = aa in (item {aa = (lo, v)}, item {aa = (v+1, hi)})
    branch A LT v = let (lo,hi) = aa in (item {aa = (v, hi)}, item {aa = (lo, v-1)})

    branch S GT v = let (lo,hi) = ss in (item {ss = (lo, v)}, item {ss = (v+1, hi)})
    branch S LT v = let (lo,hi) = ss in (item {ss = (v, hi)}, item {ss = (lo, v-1)})

    branch _ _ _ = error "branch"

branchOnFilter :: RangeItem -> String -> Map String Workflow -> [RangeItem]
branchOnFilter item label workflows =
  let workflow = fromJust $ Map.lookup label workflows in
    branch item workflow
  where
    branch _ [] = []
    branch item' (filt:filts) =
      case filt of
        Fallback act ->
          case act of
            Accept -> [item']
            Reject -> []
            Goto label' -> branchOnFilter item' label' workflows
        Filter cond act -> let (skips, takes) = branchOnCondition item' cond in
          branch skips filts ++ case act of
            Accept -> [takes]
            Reject -> []
            Goto label' -> branchOnFilter takes label' workflows

getBounds :: RangeItem -> Map String Workflow -> [RangeItem] 
getBounds item workflows = branchOnFilter item "in" workflows

isValid :: RangeItem -> Bool
isValid RangeItem{..} = and $ map (\(lo,hi) -> lo <= hi) [xx, mm, aa, ss]

noCombinations :: RangeItem -> Int
noCombinations RangeItem{..} =
  foldr (\(lo,hi) acc -> acc * (hi-lo+1)) 1 [xx, mm, aa, ss]

main :: IO ()
main = do
  (workflows', items') <- T.breakOn "\n\n" . T.pack <$> getContents
  let (workflows'', items'') = (T.lines workflows', T.lines (T.strip items'))

  let workflows = map parseWorkflow workflows''
  let items = map parseItem items''

  -- part 1
  let workflowMap = Map.fromList workflows
  let accepted = filterParts items workflowMap 
  let scores = map (\Item{..} -> x+m+a+s) accepted
  print $ sum scores

  -- part 2
  let rangeit = RangeItem {
    xx = (1, 4000),
    mm = (1, 4000),
    aa = (1, 4000),
    ss = (1, 4000)
  }
  let bounds = getBounds rangeit workflowMap
  let valid = filter isValid bounds
  let combinations = map noCombinations valid
  print $ sum combinations

