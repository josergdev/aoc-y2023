module Utils(readLines, multiMatchRegex, matchRegex, matchRegexToInt, section) where

import Text.RegexPR (multiMatchRegexPR, matchRegexPR)
import Data.Maybe (mapMaybe, listToMaybe)
import Text.Read (readMaybe)

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

multiMatchRegex :: String -> String -> [String]
multiMatchRegex p s = map snd (mapMaybe (listToMaybe . snd) (multiMatchRegexPR p s))

matchRegex :: String -> String -> Maybe String
matchRegex p l = fmap snd ((listToMaybe . snd) =<< matchRegexPR p l)

matchRegexToInt :: String -> String -> Maybe Int
matchRegexToInt p l = readMaybe =<< matchRegex p l


section :: b -> a -> (a, b)
section = flip (,)
