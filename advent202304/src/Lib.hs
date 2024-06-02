module Lib (
    someFunc,
    advent,
) where

import Debug.Trace
import Utils (removeChar, splitString)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

oneOrDouble :: Int -> String -> Int
oneOrDouble 0 _ = 1
oneOrDouble x _ = x * 2

singleGameWins :: String -> String -> Int
singleGameWins winningNumbers numbers =
    do
        let winning = splitString ' ' winningNumbers
        let numb = splitString ' ' numbers

        let score = foldl oneOrDouble 0 $ filter (\x -> x `elem` numb) winning
        score

advent :: IO ()
advent = do
    input <- readFile "input1-prod.txt"
    let lined = lines input
    let ww = map (concat . map (splitString '|') . tail . splitString ':') lined
    -- print ww

    let aa = map (\x -> singleGameWins (x !! 0) (x !! 1)) ww
    print ("answer: " ++ show (sum aa))
    print "advtent"
