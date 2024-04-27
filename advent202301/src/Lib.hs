module Lib
    ( someFunc
    ) where

import Text.Read

first :: String -> Int
first [] = -1
first (x:xs) =
    let maybe = readMaybe [x] :: Maybe Int
    in case maybe of
        Just x -> x
        Nothing -> first xs

reverse' :: String -> String
reverse' x = foldl (\acc x -> x:acc) "" x

someFunc :: IO ()
someFunc = do
    input <- readFile "actual-input.txt"
    let v1 = do
            map (first . reverse') . lines
            $ input
    let v2 = do
            map ((*10) . first) . lines
            $ input
    let zipped = zipWith (+) v1 v2
    print (zipWith (+) v1 v2)
    print (sum zipped)
