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


someFunc :: IO ()
someFunc = do
    input <- readFile "input1-test.txt"
    let formatted = format input

    print ("first: " ++ show formatted)

    let gameNumbers =  map getGameNumber $ formatted
    
    print gameNumbers

    -- print input
    print "Goodbye"
