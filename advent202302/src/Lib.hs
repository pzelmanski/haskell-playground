module Lib (
    someFunc,
) where

import Data.Maybe
import Debug.Trace
import Utils (removeChar, splitString)

-- getGameNumber :: [String] -> Int
-- getGameNumber [] = 0
-- getGameNumber game =
--     read
--         . removeChar ':'
--         $ game !! 2 ::
--         Int

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

data SingleDraft = SingleDraft
    { draftRed :: Red
    , draftGreen :: Green
    , draftBlue :: Blue
    }
    deriving (Show)

getGame :: Game
getGame = Game (GameNumber 6) (Red 4) (Green 3) (Blue 2)

strEq :: String -> String -> Bool
strEq [] [] = True
strEq [] [_] = False
strEq [_] [] = False
strEq (a : as) (b : bs) = (a == b) && strEq as bs

getGameNumber gameNumberPart = do
    let ww = (last (gameNumberPart))
    let gameNumber = read (head . reverse . words $ ww) :: Int
    gameNumber

listToTouple :: [String] -> (Int, String)
listToTouple [x, y] = (read x :: Int, y)
listToTouple _ = (0, "")


getDraft colors = do
    -- trace ("COLORS: " ++ show colors) $
    --     SingleDraft (Red 0) (Green 0) (Blue 0)
    let ww = splitString ',' colors
    let kk = map (listToTouple . words) ww
    -- TODO: I have a list of tuples (count,color)
    -- I need to re-list this to have (count,color) per color
    -- then do fold to have max per color
    let reds = listToMaybe $ filter (strEq "red" . snd) kk
    let greens = listToMaybe $ filter (strEq "green" . snd) kk
    let blues = listToMaybe $ filter (strEq "blue" . snd) kk
    trace
        ("===colors: " ++ show reds)
        SingleDraft
        ( Red (maybe 0 fst reds ))
        ( Green (maybe 0 fst greens))
        ( Blue (maybe 0 fst blues))

-- getSingleGame :: String -> Game
getSingleGame game = do
    let split = map (splitString ';') (splitString ':' game)
    let gameNumber = getGameNumber (head split)
    let colorsPart = head (tail split)
    let drafted = trace ("colors part: " ++ show colorsPart) $ map (getDraft) colorsPart
    let maxRed = draftRed (head drafted)
    let maxGreen = draftGreen (head drafted)
    let maxBlue = draftBlue (head drafted)
    Game (GameNumber gameNumber) maxRed maxGreen maxBlue

-- trace ("end: " ++ show game) $ game

-- getGames :: [String] -> [Game]
getGames games = do
    trace ("++" ++ show games) $ map getSingleGame games

someFunc :: IO ()
someFunc = do
    input <- readFile "input1-test.txt"
    let lined = lines input
    print lined
    print ""
    print ""

    print ((getGames (lined)))

    -- print ""
    -- print (show input)
    -- print "//"

    let formatted = format input

    -- let gameNumbers = map getGameNumber formatted

    -- print gameNumbers

    -- print input
    print "Goodbye"
