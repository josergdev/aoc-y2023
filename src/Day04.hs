{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Day04 where

import Utils (readLines, matchRegexToInt, matchRegex)
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)
import qualified Data.Map.Strict as M


d04p01 :: IO ()
d04p01 = do
  contents <- readLines "input/d04.txt"
  print "Day 04 - Part 1:"
  print $ exec1 contents

data Card = Card {cardId :: Int, winning :: [Int], owned :: [Int] } deriving (Ord, Eq, Show)


parseCardId :: String -> Maybe Int
parseCardId = matchRegexToInt "Card[[:space:]]*([[:digit:]]*):.*\\|.*"

parseNumbers :: String -> [Int]
parseNumbers = mapMaybe readMaybe . words

parseWinning :: String -> [Int]
parseWinning = concatMap parseNumbers . matchRegex "Card[[:space:]]*[[:digit:]]*:(.*)\\|.*"

parseOwned :: String -> [Int]
parseOwned = concatMap parseNumbers . matchRegex "Card[[:space:]]*[[:digit:]]*:.*\\|(.*)"

parseLine :: String -> Maybe Card
parseLine l = fmap (\ci -> Card {cardId=ci, winning=parseWinning l, owned=parseOwned l}) (parseCardId l)

parseLines :: [String] -> [Card]
parseLines = mapMaybe parseLine

winningOwned :: Card -> [Int]
winningOwned c = filter (`elem` winning c) (owned c)

points :: Int -> Int
points 0 = 0
points n = 2 ^ (n - 1)

sumPoints :: [Card] -> Int
sumPoints = sum . map (points . length . winningOwned)

exec1 :: [String] -> Int
exec1 = sumPoints . parseLines

d04p02 :: IO ()
d04p02 = do
  contents <- readLines "input/d04.txt"
  print "Day 04 - Part 2:"
  print "Too Long: 9425061"

cardsById :: [Card] -> M.Map Int Card
cardsById = M.fromList . map (\c -> (cardId c, c))

winningCards :: [Card] -> [(Int, Int)]
winningCards = filter ((/=0) . snd) . map (\c -> (cardId c, length (winningOwned c)))

copies :: [Card] -> [Card]
copies cs = concatMap (mapMaybe (\i -> M.lookup i (cardsById cs)) . (\c -> [(1 + fst c)..(uncurry (+) c)])) (winningCards cs)

winningCardsAndCopies :: ([Card], [Card]) -> ([Card], [Card])
winningCardsAndCopies (cx, []) = (cx, [])
winningCardsAndCopies (cx, cs) = winningCardsAndCopies (cx ++ cs, copies cs)

exec2 :: [Card] -> Int
exec2 cs = length . fst $ winningCardsAndCopies ([], cs)