{-# LANGUAGE LambdaCase #-}

module Lib (
    someFunc,
) where

import Convert (process)
import Data.Time.Clock (getCurrentTime)
import Html
import System.Directory (doesFileExist)
import System.Environment (getArgs)

confirm :: IO Bool
confirm = do
    putStrLn "The file already exists, do you want to override? [y/n]"
    getLine
        >>= \case
            "y" -> pure True
            "n" -> pure False
            _ -> putStrLn "Invalid answer, please use [y/n]" *> confirm

whenIO :: IO Bool -> IO () -> IO ()
whenIO condition action =
    condition
        >>= \case
            True -> action
            False -> pure ()

someFunc :: IO ()
someFunc = do
    -- get args
    -- match on args:
    -- zero args: get input from stdin, output into stdin
    -- two args: first one is input file, second one is output file
    -- if output file already exists, ask if override
    -- any other input: write a generic message explaining proper usage

    -- fmap
    vv <- getArgs
    case vv of
        -- getContents works in a way that I need to press ctrl-D in order to close the input stream and see the result
        [] -> getContents >>= \input -> putStrLn (process "Title" input) -- get input from stdin, output into stdin
        [inputFile, outputFile] ->
            let
                processFile = readFile inputFile >>= \input -> writeFile outputFile $ process "title" input
            in
            doesFileExist outputFile >>= \exists ->
                if exists
                    then
                        confirm >>= \answer ->
                            if answer
                                then
                                    processFile
                                else
                                    putStrLn "Answer was dont override, skipping"
                    else processFile
        _ -> putStrLn "message"

    time <- getCurrentTime

    putStrLn
        ( render $
            html_
                "My page title"
                ( p_ "My page content."
                    <> ( p_ "</br>Timestamp: </br>"
                            <> h1_ (show time)
                       )
                    <> ul_
                        [ p_ "item 1"
                        , p_ "item 2"
                        , p_ "item 3"
                        ]
                )
        )

-- 3.6 done
