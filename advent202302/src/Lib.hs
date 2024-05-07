module Lib
    ( someFunc
    ) where

removeChar :: Char -> String -> String
removeChar c xs = [x | x <- xs, not (x `elem` [c])] 

getGameNumber :: [String] -> Int
getGameNumber [] = 0
getGameNumber game = 
                read 
                . removeChar ':'             
                $ game !! 2 :: Int

readAndFormat :: String -> IO([[String]])
readAndFormat filename = do
                    input <- readFile filename
                    let result = do
                            map words
                            . lines
                            $ input

                    result

someFunc :: IO ()
someFunc = do
        input <- readFile "input1-test.txt"
        let result = do
                map words
                . lines
                $ input

        -- let gameNumbers = result $ map getGameNumber

        print result

        print input
        print "Goodbye"
