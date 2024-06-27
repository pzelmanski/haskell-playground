module Exercises where

import Html.Internal
import Prelude hiding (even, odd, replicate)


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


replicate :: Int -> a -> [a]
replicate n e = 
    if n <= 0   
        then [] 
        else e : replicate (n-1) e


odd :: Int -> Bool
odd x = 
    if x == 0 
        then False
        else even (x - 1)

even :: Int -> Bool
even x = 
    if x == 0
        then True
        else odd (x - 1)
