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

        let score = foldl oneOrDouble 0 $ filter (`elem` numb) winning
        score

-- this approach wont work for second star, its incorrect
secondStar :: String -> String -> Int
secondStar winningNumbers numbers = do
    let winning = splitString ' ' winningNumbers
    let numb = splitString ' ' numbers
    let won = filter (`elem` numb) winning
    length won

ssInput :: [Int] -> [String] -> [String]
ssInput [] [] = []
ssInput (c : cs) (x : xs) = replicate c x ++ ssInput cs xs

advent :: IO ()
advent = do
    input <- readFile "input1-test.txt"
    let lined = lines input
    let ww = map (concat . map (splitString '|') . tail . splitString ':') lined
    -- print ww

    let aa = map (\x -> singleGameWins (x !! 0) (x !! 1)) ww
    let bb = map (\x -> secondStar (x !! 0) (x !! 1)) ww
    let ss = ssInput bb lined
    print ("ss input: " ++ show (length ss))
    print ("second star: " ++ show (bb))
    print ("answer: " ++ show (sum aa))
    print "advtent"
