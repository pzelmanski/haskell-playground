module Lib
    ( someFunc
    ) where

import System.Random 

add :: Int -> Int -> Int
add x y = x + y

inc :: Int -> Int
inc x = x + 1

addInc :: Int -> Int -> Int
addInc = add . inc

data Choice = Rock | Paper | Scissors
    deriving (Show, Eq)

someFunc :: IO ()
someFunc = do
        let uniformR = uniformR (0, 7)
        let result = inc $ add 1 2 
        let result2 = addInc 3 3
        putStrLn ("someFunc result: " ++ show result ++ " || " ++ show result2 ++ " || " ++ show uniformR)

