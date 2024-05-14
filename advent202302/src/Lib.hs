module Lib (
    someFunc,
) where

import Utils (removeChar, splitString)

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

newtype GameNumber = GameNumber {gnumber :: Int} deriving (Show)
newtype Red = Red {red :: Int} deriving (Show)
newtype Green = Green {green :: Int} deriving (Show)
newtype Blue = Blue {blue :: Int} deriving (Show)

data Game = Game
    { gameNumber :: GameNumber
    , maxRed :: Red
    , maxGreen :: Green 
    , maxBlue :: Blue
    }
    deriving (Show)

getGame :: Game
getGame = Game (GameNumber 6) (Red 4) (Green 3) (Blue 2)


getSingleGame :: String -> Game
getSingleGame game = do
    let split = map (splitString ';') (splitString ':' game)
    getGame
        
    

getGames :: [String] -> [Game]
getGames games = do
    map getSingleGame games 

someFunc :: IO ()
someFunc = do
    input <- readFile "input1-test.txt"
    
    print (show (getGames (lines input)))

    print (show input)
    print "//"

    print (show getGame)
    print "//"

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
