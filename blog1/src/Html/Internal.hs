module Html.Internal where

import Numeric.Natural

-- * Types
newtype Html = Html String
newtype Structure = Structure String
type Title = String
type Body = String

type Document = [Structure2]
data Structure2
    = Heading Natural String
    | Paragraph String
    | UnorderedList [String]
    | OrderedList [String]
    | CodeBlock [String]
    deriving (Show)

example1 :: Document
example1 = 
    [ Paragraph "Hello, world!"
    ]

example2 :: Document
example2 = 
    [ Heading 1 "Welcome"
    , Paragraph "To this tutorial about Haskell." 
    ]

example3 :: Document
example3 = 
    [ Paragraph "Remember that multiple lines with no separation are grouped together into a single paragraph but list items remain separate."
    , OrderedList 
        [ "Item 1 of a list", "Item2 of the same list" 
        ] 
    ]

example4 :: Document
example4 = 
    [ Heading 1 "Compiling programs with ghc"
    , Paragraph "Running ghc invoked the Glasgow Haskell Compiler (GHC), and can be used to compile Haskell modules and programs into native executables and libraries"
    , Paragraph "Create a new Haskell source file named hello.hs, and write the following code in it:"
    , CodeBlock 
      [ "main = pusStrLn \"Hello, Haskell!\"" 
      ]
    , Paragraph "Now, we can compile the program by invoking ghc with the file name:"
    , CodeBlock 
        [ "-> ghc helo.hs"
        , "[1 of 1] Compiling main"
        , "Linking hello ..." 
        ]
    , Paragraph "GHC created the following files:"
    , UnorderedList 
        [ "hello.hi - Haskell interface file"
        , "Hello.o - Object file, the output of the compiler before linking"
        , "hello (or hello.exe on Microsoft Windows) - A native runnable executable." 
        ]
    , Paragraph "GHC will produce an executable when the source file satisfies both conditions:"
    , OrderedList 
        [ "Defines the main function in the source file"
        , "Defines the module name to be Main or does not have a module declaration" 
        ]
    , Paragraph "Otherwise, it will only produce the .o and .hi files." 
    ]


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

ul_ :: [Structure] -> Structure
ul_  = Structure . el "ul" . concatMap (el "li" . getStructureString) 

ol_ :: [Structure] -> Structure
ol_ = Structure . el "ol" . concatMap (el "li" . getStructureString)

code_ :: String -> Structure
code_ = Structure . el "pre" . escape

-- * Render
render :: Html -> String
render (Html h) = h


-- * Utils
append_ :: Structure -> Structure -> Structure
append_ (Structure s1) (Structure s2) = Structure (s1 <> s2)

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
