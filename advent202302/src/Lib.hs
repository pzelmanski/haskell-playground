module Lib (
    someFunc,
) where

import Utils (splitString, removeChar)

getGameNumber :: [String] -> Int
getGameNumber [] = 0
getGameNumber game =
    read
        . removeChar ':'
        $ game !! 2 ::
        Int

format :: String -> [[String]]
format input = do
    let result =
            do
                map words
                . lines
                $ input
    result

data Game = Game
    { gameNumber :: Int
    , maxRed :: Int
    , maxGreen :: Int
    , maxBlue :: Int
    }
    deriving (Show)



someFunc :: IO ()
someFunc = do
    input <- readFile "input1-test.txt"

    let games = lines input

    let bm = map (splitString ';') (games >>= splitString ':')
    print ("bm: " ++ show bm)
    print "//"

    let mapmap = map (map (splitString ':') . splitString ';') games
    print ("mapmap: " ++ show mapmap)
    print "//"

    let formatted = format input

    -- print ("first: " ++ show formatted)

    let gameNumbers = map getGameNumber formatted

    print gameNumbers

    -- print input
    print "Goodbye"
