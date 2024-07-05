module Html.Internal where

import Numeric.Natural

-- * Types
newtype Html = Html String
newtype Structure = Structure String
type Title = String
type Body = String

-- * EDSL
html_ :: Title -> Structure -> Html
html_ title content =
    (Html . el "html") $
        el "head" $
            el "title" (escape title) <> getStructureString (body_ content)

body_ :: Structure -> Structure
body_ content = Structure $ "<body style='background-color:grey'>" <> getStructureString content <> "</body>"

p_ :: String -> Structure
p_ = Structure . el "p" . escape

h1_ :: String -> Structure
h1_ = Structure . el "h1" . escape

h_ :: Natural -> String -> Structure
h_ num = Structure . el ("h" <> show num) . escape

ul_ :: [Structure] -> Structure
ul_ = Structure . el "ul" . concatMap (el "li" . getStructureString)

ol_ :: [Structure] -> Structure
ol_ = Structure . el "ol" . concatMap (el "li" . getStructureString)

code_ :: String -> Structure
code_ = Structure . el "pre" . escape

empty_ :: Structure
empty_ = Structure ""

-- * Render
render :: Html -> String
render (Html h) = h

-- * Utils

instance Semigroup Structure where
    (<>) (Structure s1) (Structure s2) = Structure (s1 <> s2)

instance Monoid Structure where
    mempty = empty_

el :: String -> String -> String
el tag content =
    "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

elNested :: String -> Structure -> String
elNested tag content =
    "<" <> tag <> ">" <> getStructureString content <> "</>" <> tag <> ">"

getStructureString :: Structure -> String
getStructureString (Structure s) = s

escape :: String -> String
escape =
    let escapeChar c =
            case c of
                '<' -> "&lt;"
                '>' -> "&gt;"
                '&' -> "&amp;"
                '"' -> "&quot;"
                '\'' -> "&#39;"
                _ -> [c]
     in concat . map escapeChar

concatStructure :: [Structure] -> Structure
concatStructure s =
    case s of
        [] -> empty_
        x : xs -> x <> concatStructure xs
