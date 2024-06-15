module Lib (
    someFunc,
    answer,
) where

import Debug.Trace
import Text.Read (readMaybe)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

trueForInt :: String -> Bool
trueForInt input = trace ("trueForInt: " ++ input) $ case (readMaybe input :: Maybe Int) of
    Just _ -> True
    Nothing -> False

trueForList :: (String -> String) -> String -> Bool
trueForList fn input = trueForInt $ fn input

getGroupNumbers :: String -> [String] -> [String]
getGroupNumbers name remainder = do
    if head remainder == name
        then
            trace
                ("takeNumbers: " ++ name ++ " " ++ show remainder)
                takeWhile
                (trueForList (head . words))
                (tail remainder)
        else
            trace ("takeNumbers rec: " ++ name ++ " " ++ show remainder) $ getGroupNumbers name (tail remainder)

--
data CategoryType = CategoryType
    { destRangeStart :: Int
    , sourceRangeStart :: Int
    , range :: Int
    }
    deriving (Show)

-- findMatching :: Int -> Int -> [CategoryType] -> Int
-- findMatching rangeStart rangeEnd (x:xs) = do
-- from x, find matching element to range
-- for each matching, call rec
-- findMatching (x.start, x.end, xs)
-- if single element list, return value

splitAndMap :: (a -> Bool) -> (a -> b) -> [a] -> [[b]]
splitAndMap pred mapfn input = snm input []
  where
    snm [] acc = [acc]
    snm (x : xs) acc
        | pred x = reverse acc : snm xs []
        | otherwise = snm xs (mapfn x : acc)

--matchCategories :: [[CategoryType]] -> [Int]

answer :: IO ()
answer = do
    input <- readFile "input-test.txt"
    print input
    let lns = lines input
    print "------"
    print "------"
    print "------"
    print "------"

    let seedsLine = head lns
    let otherLines = filter (/= "") $ tail lns

    let split = words seedsLine ++ otherLines
    -- print (readMaybe "123" :: Maybe Int)

    -- print ("Other lines: " ++ show (parseOtherLines otherLines))

    let ct =
            splitAndMap
                (\x -> last x == ':')
                ( \x ->
                    (\[x, y, z] -> CategoryType x y z) $
                        (map read $ words x :: [Int])
                )
                otherLines
    print ct
    -- format of data:
    -- destination range start; source range start; range

    -- TODO: Get numbers for each group
    -- print (getGroupNumbers "light-to-temperature map:" split)
    -- actually, the name are irrelevant - if I can assume that they're
    -- sorted in a correct order in input, I'm only interested in the
    -- last number, so I can make reading way more generic
    -- let lightToTemp = (getGroupNumbers "light-to-temperature map:" split)
    -- let ints = map (\x -> map read $ words x :: [Int]) lightToTemp
    -- let datas = map (\[x,y,z] -> CategoryType x y z) ints
    -- here i have a list of first caterory
    -- i need to match it with previous
    -- and then match the result of this one with next
    -- print datas

    print "Goodbye"
