module Lib
    ( someFunc
    ) where

import System.Random (randomR, getStdGen)

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n-1)


someFunc :: IO ()
someFunc = do
    g <- getStdGen
    let (rand, _) = randomR (1, 20) g
    let v = factorial rand
    putStrLn ("Factorial of " ++ show rand ++ " is " ++ show v)
