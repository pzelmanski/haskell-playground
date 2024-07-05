module Convert where

import qualified Html
import qualified Parser

convertStructure :: Parser.Structure -> Html.Structure
convertStructure s =
    case s of
        Parser.Heading n txt ->
            Html.h_ n txt
        Parser.Paragraph p ->
            Html.p_ p
        Parser.UnorderedList l ->
            Html.ul_ $ map Html.p_ l
        Parser.OrderedList l ->
            Html.ol_ $ map Html.p_ l
        Parser.CodeBlock c ->
            Html.code_ (unlines c)

-- String -> [Structure] -> Html
convert :: Html.Title -> Parser.Document -> Html.Html
convert title = Html.html_ title . foldMap convertStructure

-- Html.Structure is monoid
-- List is foldable
-- foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
-- I can think of it as:
-- foldMap :: (Parser.Structure -> Html.Structure) -> [] Structure -> Html.Structure
fm :: [Parser.Structure] -> Html.Structure
fm document = foldMap convertStructure document

-- it's equivallent to
myFoldMap :: [Parser.Structure] -> Html.Structure
myFoldMap document = Html.concatStructure $ map convertStructure document

convertStructureRec :: Parser.Document -> [Html.Structure]
convertStructureRec document =
    case document of
        [] -> [Html.empty_]
        (x : xs) -> convertStructure x : convertStructureRec xs

convertAndConcat :: Parser.Document -> Html.Structure
convertAndConcat = Html.concatStructure . convertStructureRec
