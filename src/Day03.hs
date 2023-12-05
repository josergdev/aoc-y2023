{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Day03 where

import Utils (readLines)
import Data.Char (isDigit)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.List.Split (splitWhen)
import Text.Read (readMaybe)

d03p01 :: IO ()
d03p01 = do
  contents <- readLines "input/d03.txt"
  print "Day 03 - Part 1:"
  print $ exec1 contents

data Point = Point {px :: Int, py :: Int} deriving (Ord, Eq, Show)

fromTuple :: (Int, Int) -> Point
fromTuple (x,y) = Point x y

isPoint :: Char -> Bool
isPoint = (== '.')

isSymbol :: Char -> Bool
isSymbol c = not (isDigit c) && not (isPoint c)

indexed :: [String] -> [[(Point, Char)]]
indexed m = zipWith (\rs y -> zipWith (\r x -> (fromTuple (x, y), r)) rs [0..]) m [0..]

toMap :: [[(Point, Char)]] -> Map.Map Point (Point, Char)
toMap m  = Map.fromList (map (\t -> (fst t, t)) (concat m))

adsj :: Point -> [Point]
adsj (Point x y) = map fromTuple
             [ (x-1, y-1), (x, y-1), (x+1, y-1)
             , (x-1, y),             (x+1, y)
             , (x-1, y+1), (x, y+1), (x+1, y+1) ]

look :: Ord k => Map.Map k a -> k -> Maybe a
look = flip Map.lookup 

adjsOf :: Map.Map Point b -> Point -> [b]
adjsOf m xy = mapMaybe (look m) (adsj xy)

symbols :: [[(Point, Char)]] -> [(Point, Char)]
symbols = concatMap (filter (isSymbol . snd))

pointsTouched :: [[(Point, Char)]] -> [(Point, Char)]
pointsTouched m = concatMap (adjsOf (toMap m) . fst) (symbols m)

onlyDigits :: [(Point, Char)] -> [Point]
onlyDigits m = map fst (filter (isDigit . snd) m)

groupNumbers :: [[(Point, Char)]] -> [[(Point, Char)]]
groupNumbers = filter (/=[]) . splitWhen (not . isDigit . snd) . concat

parseNumbers :: [[(Point, Char)]] -> [([Char], [Point])]
parseNumbers = map (\ps -> (map snd ps, map fst ps))

anyElemIsElemOf :: (Foldable t1, Foldable t2, Eq a) => t1 a -> t2 a -> Bool
anyElemIsElemOf f1 f2 = any (`elem` f2) f1

numbersTouched :: [[(Point, Char)]] -> [([Char], [Point])]
numbersTouched m = filter (\ps -> anyElemIsElemOf (snd ps) ((onlyDigits . pointsTouched) m)) ((parseNumbers . groupNumbers) m)

sumNumbersTouched :: [String] -> Int
sumNumbersTouched =  sum . mapMaybe (readMaybe . fst) . numbersTouched . indexed

exec1 :: [String] -> Int
exec1 = sumNumbersTouched