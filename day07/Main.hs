module Main where

import Util
import Data.List

type Hand = (String, Int)

-- >>> (group . sort) "T55J5"
-- ["555","J","T"]

-- >>> group [1,2]
-- [[1],[2]]

-- >>> handRank "77773"
-- 2

-- >>> handRank "5J455"
-- 4

-- >>> handRank "AAAAA"
-- 1

-- >>> handRank "99787"
-- 5

-- >>> (sortBy (\a b -> length b `compare` length a) . group . sort) "99787"
-- ["77","99","8"]

charPrio :: Char -> Int
charPrio 'A' = 1
charPrio 'K' = 2
charPrio 'Q' = 3
charPrio 'J' = 4
charPrio 'T' = 5
charPrio '9' = 6
charPrio '8' = 7
charPrio '7' = 8
charPrio '6' = 9
charPrio '5' = 10
charPrio '4' = 11
charPrio '3' = 12
charPrio '2' = 13
charPrio e = error ("Invalid character " <> show e)

handRank :: String -> Int
handRank h =
  let grouped = (sortBy (\a b -> length b `compare` length a) . group. sort) h in
    let len = length grouped in
      if len == 1 then
        1 -- five of a kind
      else if len == 2 && (length . head) grouped == 4 then
        2 -- four of a kind
      else if len == 2 then
        3 -- full house
      else if len == 3 && (length . head) grouped == 3 then
        4 -- three of a kind
      else if len == 3 && (length . head) grouped == 2 && length (grouped !! 1) == 2 then
        5 -- two pair
      else if len == 4 then
        6 -- one pair
      else
        7 -- high card

order :: Hand -> Hand -> Ordering
order (a, _) (b, _)
  = let comp = handRank a `compare` handRank b in
    if comp == EQ then
     pairWise a b 
    else
      comp
  where
    pairWise [] _ = EQ
    pairWise _ [] = EQ
    pairWise (a:as) (b:bs)
      = let prio = charPrio a `compare` charPrio b in
          if prio == EQ then
            pairWise as bs
          else
            prio

charPrio2 :: Char -> Int
charPrio2 'A' = 1
charPrio2 'K' = 2
charPrio2 'Q' = 3
charPrio2 'T' = 4
charPrio2 '9' = 5
charPrio2 '8' = 6
charPrio2 '7' = 7
charPrio2 '6' = 8
charPrio2 '5' = 9
charPrio2 '4' = 10
charPrio2 '3' = 11
charPrio2 '2' = 12
charPrio2 'J' = 13
charPrio2 e = error ("Invalid character " <> show e)


-- >>> convert "2AJJJ"
-- "2AAAA"

-- >>> (group.sort) "JKJJ2"
-- ["2","JJJ","K"]

-- >>> map head $ filter (\g -> length g == 1) $ (group.sort) "JKJJ2"
-- "2K"

-- >>> minimumBy (\a b -> charPrio2 a `compare` charPrio2 b) "2K"
-- 'K'

-- >>> maximumBy (\a b -> if head a == 'J' then LT else if head b == 'J' then GT else length a `compare` length b) $ (group.sort) "JKJJ2"
-- "K"

-- Convert J to the card that's most common in our hand, if multiple cars
-- have the same occurrence then pick the card that's strongest
-- If all characters are 'J' then we simply turn them all into 'A's
convert :: String -> String
convert h =
  let grouped = (group . sort) h in
    let
      longest =
        maximumBy
          (\a b ->
            if head a == 'J' then
              LT
            else if head b == 'J' then
              GT
            else
              length a `compare` length b
          ) grouped in

      let similar = map head $ filter (\g -> length g == length longest) grouped in
        let strongest = minimumBy (\a b -> charPrio2 a `compare` charPrio2 b) similar in
          if strongest == 'J' then
            "AAAAA"
          else
            replace 'J' strongest h

handRank2 :: String -> Int
handRank2 h =
  let converted = convert h in
    let grouped = (sortBy (\a b -> length b `compare` length a) . group. sort) converted in
      let len = length grouped in
        if len == 1 then
          1 -- five of a kind
        else if len == 2 && (length . head) grouped == 4 then
          2 -- four of a kind
        else if len == 2 then
          3 -- full house
        else if len == 3 && (length . head) grouped == 3 then
          4 -- three of a kind
        else if len == 3 && (length . head) grouped == 2 && length (grouped !! 1) == 2 then
          5 -- two pair
        else if len == 4 then
          6 -- one pair
        else
          7 -- high card

order2 :: Hand -> Hand -> Ordering
order2 (a, _) (b, _)
  = let comp = handRank2 a `compare` handRank2 b in
    if comp == EQ then
     pairWise a b 
    else
      comp
  where
    pairWise [] _ = EQ
    pairWise _ [] = EQ
    pairWise (a:as) (b:bs)
      = let prio = charPrio2 a `compare` charPrio2 b in
          if prio == EQ then
            pairWise as bs
          else
            prio

main :: IO ()
main = do
  lines <- input

  let hands = map (\l -> ((head . words) l, (read . last . words) l :: Int)) lines

  -- part 1
  let sorted = sortBy (flip order) hands
  let ans1 = sum $ zipWith (\(_, v) r -> v * r) sorted [1..]
  print ans1


  -- part 2
  let sorted2 = sortBy (flip order2) hands
  let ans2 = sum $ zipWith (\(_, v) r -> v * r) sorted2 [1..]
  print ans2

