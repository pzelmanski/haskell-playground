module Lib
    ( someFunc
    ) where

readfile :: FilePath -> IO String
readfile path = do
  contents <- readFile path
  return contents


someFunc :: IO ()
someFunc = do
    content <- readFile "readme.txt"

    print . map length . lines $ content
