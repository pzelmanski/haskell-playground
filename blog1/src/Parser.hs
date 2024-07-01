module Parser where

import Data.Maybe (maybeToList)
import Numeric.Natural (Natural)


type Document = [Structure]
data Structure
    = Heading Natural String
    | Paragraph String
    | UnorderedList [String]
    | OrderedList [String]
    | CodeBlock [String]
    deriving (Eq, Show)




parse :: String -> Document
parse = parseLines Nothing . lines

parseLines :: Maybe Structure -> [String] -> Document
parseLines context txts =
    case txts of
        [] -> maybeToList context

        -- heading case 
        ('*': ' ': line) : rest -> maybe id (:) context (Heading 1 (trim line) : parseLines Nothing rest)

        -- unordered list case
        ('-' : ' ' : line) : rest -> 
            case context of
                Just (UnorderedList l) -> parseLines (Just (UnorderedList (l <> [trim line]))) rest
                _ -> maybe id (:) context (parseLines (Just (UnorderedList [trim line])) rest)

        -- code block case
        ('>' : ' ' : line) : rest -> 
            case context of
                Just (CodeBlock c) -> parseLines (Just (CodeBlock (c <> [trim line]))) rest
                _ -> maybe id (:) context (parseLines (Just (CodeBlock [trim line])) rest)

        -- ordered list case
        ('#' : ' ' : line) : rest ->
            case context of
                Just (OrderedList o) -> parseLines (Just (OrderedList (o <> [trim line]))) rest
                _ -> maybe id (:) context (parseLines (Just (OrderedList [trim line])) rest)


        -- paragraph case
        currentLine : rest ->
            let 
                line = trim currentLine
            in 
                if line == ""
                    then
                    -- Version with maybe, shortest
                    -- maybe id (:) context (parseLines Nothing rest)
                    
                    -- Version simplified shortened, half way to maybe
                        (case context of
                            Just p -> (:) p
                            Nothing -> id)
                            (parseLines Nothing rest)

                    -- Version simplified:
                    --    case context of
                    --        Just p -> p : parseLines Nothing rest
                    --        Nothing -> parseLines Nothing rest
                    else
                        case context of
                            Just (Paragraph p) -> 
                                parseLines (Just (Paragraph (unwords [p, line]))) rest
                            _ -> 
                                maybe id (:) context (parseLines (Just (Paragraph line)) rest)

data Context
    = CtxHeading Natural String
    | CtxParagraph [String]
    | CtxUnorderedList [String]
    | CtxOrderedList [String]
    | CtxCodeBlock [String]


trim :: String -> String
trim = unwords . words
