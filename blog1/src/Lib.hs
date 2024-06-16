module Lib (
    someFunc,
) where

import Data.Time.Clock (getCurrentTime)
import Html (html_, append_, appendStr_, render, p_, h1_)

someFunc :: IO ()
someFunc = do
    time <- getCurrentTime
    putStrLn
        ( render
            $ html_
                "My page title"
            $ appendStr_
                "My page content;"
                ( append_
                    (p_ "</br>Timestamp: </br>")
                    (h1_ (show time))
                )
        )

-- 3.5 done
