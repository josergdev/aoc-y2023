{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Day07 where

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

e2 ss = ss
