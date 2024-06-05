module Lib
    ( someFunc,
      answer
    ) where

import Text.Read (readMaybe)
import Debug.Trace


someFunc :: IO ()
someFunc = putStrLn "someFunc"


trueForInt :: String -> Bool
trueForInt input = trace ("trueForInt: " ++ input ) $ case (readMaybe input :: Maybe Int) of 
                        Just _ -> True
                        Nothing -> False

trueForList :: (String -> String) -> String -> Bool
trueForList fn input = trueForInt $ fn input

getGroupNumbers :: String -> [String] -> [String]
getGroupNumbers name remainder = do
     if head remainder == name then
        trace ("takeNumbers: " ++ name ++ " " ++ show remainder) 
        takeWhile (trueForList (head . words)) (tail remainder)
     else 
        trace ("takeNumbers rec: " ++ name ++ " " ++ show remainder) $ getGroupNumbers name (tail remainder)


answer :: IO ()
answer = do
    input <- readFile "input-test.txt"
    print input
    let lns = lines input
    
    let seedsLine = head lns 
    let otherLines = filter (/= "") $ tail lns

    let split = words seedsLine ++ otherLines
    print (readMaybe "123" :: Maybe Int)

    -- TODO: Get numbers for each group
    print (getGroupNumbers "light-to-temperature map:" split)


    print "Goodbye"
