{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Day07(d07p01, d07p02) where

import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe (maybeToList)
import qualified Data.Ord as O
import Text.Read (readMaybe)
import Utils (readLines)

d07p01 :: IO ()
d07p01 = do
  contents <- readLines "input/d07.txt"
  print "Day 07 - Part 1:"
  print $ e1 contents

data Card = CA | CK | CQ | CJ | CT | C9 | C8 | C7 | C6 | C5 | C4 | C3 | C2
  deriving (Eq, Ord, Show)

data Hand = Hand
  { cards :: [Card],
    bid :: Int
  }
  deriving (Eq, Ord, Show)

parsePairs :: [(Char, Card)]
parsePairs = [('A', CA), ('K', CK), ('Q', CQ), ('J', CJ), ('T', CT), ('9', C9), ('8', C8), ('7', C7), ('6', C6), ('5', C5), ('4', C4), ('3', C3), ('2', C2)]

parseCard :: Char -> Card
parseCard c = M.fromList parsePairs M.! c

parseCards :: String -> [Card]
parseCards = map parseCard

parseBid :: String -> Int
parseBid = head . maybeToList . readMaybe

parseHand :: [String] -> Hand
parseHand ss = Hand {cards = parseCards . head $ ss, bid = parseBid . last $ ss}

parseHands :: [String] -> [Hand]
parseHands = map (parseHand . words)

countMap :: [Card] -> M.Map Card Int
countMap = foldl (\acc c -> M.insertWith (+) c 1 acc) M.empty

toCountMap :: Hand -> M.Map Card Int
toCountMap = countMap . cards

kinds :: Hand -> [Int]
kinds = M.elems . toCountMap

count :: (Eq a) => a -> [a] -> Int
count x = length . filter (x ==)

isFiveOfAKind :: Hand -> Bool
isFiveOfAKind = (1 ==) . count 5 . kinds

isFourOfAKind :: Hand -> Bool
isFourOfAKind h = all ($ h) [(1 ==) . count 4 . kinds, (1 ==) . count 1 . kinds]

isFullHouse :: Hand -> Bool
isFullHouse h = all ($ h) [(1 ==) . count 3 . kinds, (1 ==) . count 2 . kinds]

isThreeOfAKind :: Hand -> Bool
isThreeOfAKind h = all ($ h) [(1 ==) . count 3 . kinds, (2 ==) . count 1 . kinds]

isTwoPair :: Hand -> Bool
isTwoPair h = all ($ h) [(2 ==) . count 2 . kinds, (1 ==) . count 1 . kinds]

isOnePair :: Hand -> Bool
isOnePair h = all ($ h) [(1 ==) . count 2 . kinds, (3 ==) . count 1 . kinds]

isHighCard :: Hand -> Bool
isHighCard = (5 ==) . count 1 . kinds

groupByType :: [Hand] -> [[Hand]]
groupByType hs = map (`filter` hs) [isHighCard, isOnePair, isTwoPair, isThreeOfAKind, isFullHouse, isFourOfAKind, isFiveOfAKind]

sortOnCards :: [Hand] -> [Hand]
sortOnCards = L.sortOn (O.Down . cards)

groupsSorted :: [Hand] -> [[Hand]]
groupsSorted = map sortOnCards . groupByType

ranked :: [Hand] -> Int
ranked = sum . zipWith (*) [1 ..] . map bid . concat . filter (/= []) . groupsSorted

e1 :: [String] -> Int
e1 = ranked . parseHands

d07p02 :: IO ()
d07p02 = do
  contents <- readLines "input/d07.txt"
  print "Day 07 - Part 2:"
  print $ e2 contents

newtype OrdCard2 = OrdCard2 Card deriving (Eq)

instance Ord OrdCard2 where
  compare :: OrdCard2 -> OrdCard2 -> Ordering
  compare (OrdCard2 CJ) (OrdCard2 CJ) = EQ
  compare (OrdCard2 CJ) _ = GT
  compare _ (OrdCard2 CJ) = LT
  compare (OrdCard2 oc1) (OrdCard2 oc2) = compare oc1 oc2

sortOnCards2 :: [Hand] -> [Hand]
sortOnCards2 = L.sortOn (O.Down . map OrdCard2 . cards)

kinds2 :: Hand -> [Int]
kinds2 = M.elems . M.delete CJ . toCountMap

countJ :: Hand -> Int
countJ = M.findWithDefault 0 CJ . toCountMap

noJoker :: Hand -> Bool
noJoker = (0 ==) . countJ

isFiveOfAKind2 :: Hand -> Bool
isFiveOfAKind2 h =
  isFiveOfAKind h
    || all ($ h) [(1 ==) . countJ, (1 ==) . count 4 . kinds2]
    || all ($ h) [(2 ==) . countJ, (1 ==) . count 3 . kinds2]
    || all ($ h) [(3 ==) . countJ, (1 ==) . count 2 . kinds2]
    || all ($ h) [(4 ==) . countJ, (1 ==) . count 1 . kinds2]

isFourOfAKind2 :: Hand -> Bool
isFourOfAKind2 h =
  all ($ h) [noJoker, isFourOfAKind]
    || all ($ h) [(1 ==) . countJ, (1 ==) . count 3 . kinds2, (1 ==) . count 1 . kinds2]
    || all ($ h) [(2 ==) . countJ, (1 ==) . count 2 . kinds2, (1 ==) . count 1 . kinds2]
    || all ($ h) [(3 ==) . countJ, (2 ==) . count 1 . kinds2]

isFullHouse2 :: Hand -> Bool
isFullHouse2 h = 
  all ($ h) [noJoker, isFullHouse]
    || all ($ h) [(1 ==) . countJ, (2 ==) . count 2 . kinds2]

isThreeOfAKind2 :: Hand -> Bool
isThreeOfAKind2 h =
  all ($ h) [noJoker, isThreeOfAKind]
    || all ($ h) [(1 ==) . countJ, (1 ==) . count 2 . kinds2, (2 ==) . count 1 . kinds2]
    || all ($ h) [(2 ==) . countJ, (3 ==) . count 1 . kinds2]

isTwoPair2 :: Hand -> Bool
isTwoPair2 h =
  all ($ h) [noJoker, isTwoPair]

isOnePair2 :: Hand -> Bool
isOnePair2 h =
  all ($ h) [noJoker, isOnePair]
    || all ($ h) [(1 ==) . countJ, (4 ==) . count 1 . kinds2]

isHighCard2 :: Hand -> Bool
isHighCard2 h = all ($ h) [(0 ==) . countJ, isHighCard]

groupByType2 :: [Hand] -> [[Hand]]
groupByType2 hs = map (`filter` hs) [isHighCard2, isOnePair2, isTwoPair2, isThreeOfAKind2, isFullHouse2, isFourOfAKind2, isFiveOfAKind2]

groupsSorted2 :: [Hand] -> [[Hand]]
groupsSorted2 = map sortOnCards2 . groupByType2

ranked2 :: [Hand] -> Int
ranked2 = sum . zipWith (*) [1 ..] . map bid . concat . filter (/= []) . groupsSorted2

e2 :: [String] -> Int
e2 = ranked2 . parseHands
