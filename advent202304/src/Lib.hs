module Lib (
    someFunc,
    advent,
) where

import Debug.Trace
import Utils (removeChar, splitString)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

oneOrDouble :: Int -> Int -> Int
oneOrDouble 0 _ = 1
oneOrDouble x _ = x * 2

singleGameWins :: String -> String -> Int
singleGameWins winningNumbers numbers = do
    let winning = map (\x -> read x :: Int) $ splitString ' ' winningNumbers
    let numb = map (\x -> read x :: Int) $ splitString ' ' numbers

    let common = filter (\x -> x `elem` numb) winning
    let score = foldl oneOrDouble 0 common
    score

advent :: IO ()
advent = do
    input <- readFile "input1-prod.txt"
    let lined = lines input
    let ww = map (\x -> concat $ map (splitString '|') $ tail $ splitString ':' x) lined
    print ww

    let aa = map (\x -> singleGameWins (x !! 0) (x !! 1)) ww
    print ("answer: " ++ show (sum aa))
    print "advtent"
