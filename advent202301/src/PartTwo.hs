module PartTwo 
    ( calculate)
    where

import Text.Read
import qualified Data.Text as T (pack, unpack, replace)


getFirstDigit :: String -> Int
getFirstDigit [] = -1
getFirstDigit (x:xs) =
    let maybe = readMaybe [x] :: Maybe Int
    in case maybe of
        Just y -> y
        Nothing -> getFirstDigit xs

reverse' :: String -> String
reverse' x = foldl (\acc y -> y:acc) "" x

someFunc :: IO ()
someFunc = do
    input <- readFile "actual-input.txt"
    let v1 = do
            map (getFirstDigit . reverse') . lines
            $ input
    let v2 = do
            map ((*10) . getFirstDigit) . lines
            $ input
    let zipped = zipWith (+) v1 v2
    print (zipWith (+) v1 v2)
    print (sum zipped)

replace' :: String -> String
replace' = T.unpack 
            -- replacement is done in such strange way because letters might overlap: oneight is 18.
            . T.replace (T.pack "one") (T.pack "o1e") 
            . T.replace (T.pack "two") (T.pack "t2o") 
            . T.replace (T.pack "three") (T.pack "t3e") 
            . T.replace (T.pack "four") (T.pack "f4r") 
            . T.replace (T.pack "five") (T.pack "f5e") 
            . T.replace (T.pack "six") (T.pack "s6x") 
            . T.replace (T.pack "seven") (T.pack "s7n") 
            . T.replace (T.pack "eight") (T.pack "e8t") 
            . T.replace (T.pack "nine") (T.pack "n9e") 
            . T.pack 


calculate = do
    input <- readFile "input-2-actual.txt"
    let ww = replace' input
    print ww

    let v1 = do
            map (getFirstDigit . reverse') . lines
            $ ww 
    let v2 = do
            map ((*10) . getFirstDigit) . lines
            $ ww
    let zipped = zipWith (+) v1 v2
    print (zipWith (+) v1 v2)
    print (sum zipped)
    print "end" 
