module Utils (splitString, removeChar) where

splitString :: Char -> String -> [String]
splitString _ [] = []
splitString splitBy xs =
    if head xs == splitBy
        then splitString splitBy (tail xs)
        else
            e : splitString splitBy es
  where
    (e, es) = break (== splitBy) xs

removeChar :: Char -> String -> String
removeChar c xs = [x | x <- xs, x /= c]
