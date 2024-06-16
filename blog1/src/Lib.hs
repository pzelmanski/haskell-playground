module Lib (
    someFunc,
) where

import Data.Time.Clock (getCurrentTime)

someFunc :: IO ()
someFunc = do
    time <- getCurrentTime
    putStrLn
        ( render
            $ makeHtml
                "My page title"
            $ append_
                (Structure "My page content;")
                ( append_
                    (p_ "</br>Timestamp: </br>")
                    (h1_ (show time))
                )
        )

makeHtml :: Title -> Structure -> Html
makeHtml title content =
    (Html . el "html") $
        el "head" $
            el "title" title <> getStructureString (body_ content)

body_ :: Structure -> Structure
body_ content = Structure $ "<body style='background-color:grey'>" <> getStructureString content <> "</body>"

p_ :: String -> Structure
p_ = Structure . el "p"

h1_ :: String -> Structure
h1_ = Structure . el "h1"

el :: String -> String -> String
el tag content =
    "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

-- 3.3 finished

newtype Html = Html String
newtype Structure = Structure String
type Title = String

append_ :: Structure -> Structure -> Structure
append_ (Structure s1) (Structure s2) = Structure (s1 ++ s2)

getStructureString :: Structure -> String
getStructureString (Structure s) = s

render :: Html -> String
render (Html h) = h

-- 3.4 finished
