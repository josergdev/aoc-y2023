{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Day01 where

import Utils (readLines, multiMatchRegex)
import Data.Char (isDigit)
import Text.Read (readMaybe)
import Data.Maybe (mapMaybe)

d01p01 :: IO ()
d01p01 = do
    contents <- readLines "input/d01.txt"
    print "Day 01 - Part 1:"
    print $ exec1 contents

exec1 :: [String] -> Int
exec1 = sum . mapMaybe (readMaybe . headAndLast . filter isDigit)
    where 
        headAndLast s = [head s, last s]

d01p02 :: IO ()
d01p02 = do
    contents <- readLines "input/d01.txt"
    print "Day 01 - Part 2:"
    print $ exec2 contents

exec2 :: [String] -> Int
exec2 = sum . mapMaybe (readMaybe . headAndLast . filterDigitsAndParse)
    where 
        headAndLast s = [head s, last s]

filterDigitsAndParse :: String -> String
filterDigitsAndParse s = map parse (filter (/=[]) (filterDigits s))
    where
        filterDigits :: String -> [String]
        filterDigits = multiMatchRegex "(?=([[:digit:]]|one|two|three|four|five|six|seven|eight|nine))"

        parse :: String -> Char
        parse "one" = '1'
        parse "two" = '2'
        parse "three" = '3'
        parse "four" = '4'
        parse "five" = '5'
        parse "six" = '6'
        parse "seven" = '7'
        parse "eight" = '8'
        parse "nine" = '9'
        parse ss = head ss
        