module Lib (
    someFunc,
) where

import Data.Time.Clock (getCurrentTime)
import Html

someFunc :: IO ()
someFunc = do
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
