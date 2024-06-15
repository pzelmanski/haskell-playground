module Lib (
    someFunc,
) where

import Data.Time.Clock (getCurrentTime)

someFunc :: IO ()
someFunc = do
    time <- getCurrentTime
    putStrLn
        ( makeHtml
            "My page title"
            $ "My page content;"
                <> p_ "</br>Timestamp: </br>"
                <> h1_ (show time)
        )

makeHtml :: String -> String -> String
makeHtml title content = html_ $ head_ $ title_ title <> body_ content

html_ :: String -> String
html_ = el "html"

body_ :: String -> String
body_ content = "<body style='background-color:grey'>" <> content <> "</body>"

head_ :: String -> String
head_ = el "head"

title_ :: String -> String
title_ = el "title"

p_ :: String -> String
p_ = el "p"

h1_ :: String -> String
h1_ = el "h1"

el :: String -> String -> String
el tag content =
    "<" <> tag <> ">" <> content <> "</" <> tag <> ">"
