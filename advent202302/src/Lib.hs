module Lib (
    someFunc,
) where

removeChar :: Char -> String -> String
removeChar c xs = [x | x <- xs, not (x `elem` [c])]

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

getGames :: String -> [String]
getGames input = lines $ input

data Game = Game
    { gameNumber :: Int
    , maxRed :: Int
    , maxGreen :: Int
    , maxBlue :: Int
    }
    deriving (Show)

splitString :: Char -> String -> [String]
splitString _ [] = []
splitString splitBy xs =
    if head xs == splitBy
        then splitString splitBy (tail xs)
        else
            e : splitString splitBy es
  where
    (e, es) = break (== splitBy) xs

-- getMax :: String -> String -> Int
-- getMax color game = do
-- split game by :, take tail
-- split remainder by ;
-- split each individual list by ,
-- check the color of each element
-- if color matches, take the number
-- do max of numbers
--    game

someFunc :: IO ()
someFunc = do
    input <- readFile "input1-test.txt"

    let games = getGames input
    -- print ("Games: " ++ show games)

    let split =
            map (splitString ';')
                . concatMap (splitString ':')
                $ games
    -- print (show split)

    let split2 = games >>= (splitString ';') >>= (splitString ':')
    -- print ("Split2: " ++ show split2)

    -- let split3 = map (splitString ';') . ((splitString ':') >>=) $ games

    let bind = games >>= (splitString ':')
    print ("bind: " ++ show bind)

    let mapp = map (splitString ';') $ bind
    print ("mapp: " ++ show mapp)

    let bm = map (splitString ';') $ (games >>= (splitString ':'))
    print ("bm: " ++ show bm)

    let mapmap = map (map (splitString ':')) . map (splitString ';') $ games
    print ("mapmap: " ++ show mapmap)

    let formatted = format input

    -- print ("first: " ++ show formatted)

    let gameNumbers = map getGameNumber $ formatted

    print gameNumbers

    -- print input
    print "Goodbye"
