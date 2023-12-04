{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Day02 where

import Data.List.Split (splitOn)
import Data.Maybe (catMaybes, mapMaybe)
import Utils (readLines, matchRegex, matchRegexToInt)
import Data.List ( find, maximumBy )
import Data.Function

d02p01 :: IO ()
d02p01 = do
  contents <- readLines "input/d02.txt"
  print "Day 02 - Part 1:"
  print $ exec1 contents

data Color = Red | Green | Blue deriving (Show, Eq)

data Game = Game { gameId :: Int, sets :: [[(Int, Color)]] } deriving (Show)

parseGameId :: String -> Maybe Int
parseGameId = matchRegexToInt "Game ([[:digit:]]*):.*"

parseSets :: String -> [[(Int, Color)]]
parseSets l = maybe [] (map parseSet . splitOn ";") (matchRegex "Game [[:digit:]]*:(.*)" l)

parseSet :: String -> [(Int, Color)]
parseSet s = catMaybes [cParse Red "red" s, cParse Green "green" s, cParse Blue "blue" s]
    where
        cParse c sc ss = fmap (section c) (matchRegexToInt ("([[:digit:]]*) " ++ sc) ss)
        section = flip (,)

parseGame :: String -> Maybe Game
parseGame s = fmap (\i -> Game i (parseSets s)) (parseGameId s)

data BagLimits = BagLimits
  { red :: Int,
    green :: Int,
    blue :: Int
  }

initialBagLimits :: BagLimits
initialBagLimits = BagLimits {red=12, green=13, blue=14}

colorLimit :: Color -> Int
colorLimit Red = red initialBagLimits
colorLimit Green = green initialBagLimits
colorLimit Blue = blue initialBagLimits

isPossibleSet :: [(Int, Color)] -> Bool
isPossibleSet s = isP Red && isP Green && isP Blue
    where
        cFind c = find ((== c) . snd) s
        cLimit c b = fst b <= colorLimit c
        isP c = maybe True (cLimit c) (cFind c)

isPossibleGame :: Game -> Bool
isPossibleGame = all isPossibleSet . sets

collect1 :: [Game] -> Int
collect1 = sum . map gameId . filter isPossibleGame

exec1 :: [String] -> Int
exec1 = collect1 . mapMaybe parseGame

d02p02 :: IO ()
d02p02 = do
  contents <- readLines "input/d02.txt"
  print "Day 02 - Part 2:"
  print $ exec2 contents

minimumSet :: [[(Int, Color)]] -> [(Int, Color)]
minimumSet ss = [minColor Red, minColor Green, minColor Blue]
  where
    filterColor c = filter ((==c) . snd) (concat ss)
    minOrZero c [] = (0,c)
    minOrZero _ ics = maximumBy (compare `on` fst) ics
    minColor c = minOrZero c (filterColor c)

powSet :: Game -> Int
powSet = product . map fst . minimumSet . sets

collect2 :: [Game] -> Int
collect2 = sum . map powSet

exec2 :: [String] -> Int
exec2 = collect2 . mapMaybe parseGame