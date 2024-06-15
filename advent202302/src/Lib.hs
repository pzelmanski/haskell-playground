module Lib (
    someFunc,
) where

import Data.Maybe
import Debug.Trace
import Utils (removeChar, splitString)

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

getBaseGame :: Game
getBaseGame = Game (GameNumber 6) (Red 12) (Green 13) (Blue 14)

isGamePossible :: Game -> Game -> Bool
isGamePossible base game = (red . maxRed $ base) >= (red . maxRed $ game)
    && (blue . maxBlue $ base) >= (blue . maxBlue $ game)
    && (green . maxGreen $ base) >= (green . maxGreen $ game)

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
    let ww = splitString ',' colors
    let kk = map (listToTouple . words) ww
    let reds = listToMaybe $ filter (strEq "red" . snd) kk
    let greens = listToMaybe $ filter (strEq "green" . snd) kk
    let blues = listToMaybe $ filter (strEq "blue" . snd) kk
    SingleDraft
        ( Red (maybe 0 fst reds ))
        ( Green (maybe 0 fst greens))
        ( Blue (maybe 0 fst blues))

getSingleGame :: String -> Game
getSingleGame game = do
    let split = map (splitString ';') (splitString ':' game)
    let gameNumber = getGameNumber (head split)
    let colorsPart = head (tail split)
    let drafted = trace ("colors part: " ++ show colorsPart) $ map (getDraft) colorsPart
    let maxRed = foldr max 0 $ map (red . draftRed) drafted 
    let maxGreen = foldr max 0 $ map (green . draftGreen) drafted
    let maxBlue = foldr max 0 $ map (blue . draftBlue) drafted
    Game (GameNumber gameNumber) (Red maxRed) (Green maxGreen) (Blue maxBlue)

getGames games = do
    map getSingleGame games

someFunc :: IO ()
someFunc = do
    input <- readFile "input1-prod.txt"
    let lined = lines input

    let games = getGames lined
    print (getGames lined)

    let ww = filter (isGamePossible getBaseGame) games
    let oo = map (gnumber . gameNumber) ww
    print (sum oo)

    print "Goodbye"
