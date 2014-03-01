-- Test for new version of the parse function

phrases =
    [ (\x -> case x of { ("go":"to":room:_) -> Just room; (_) -> Nothing })
    , (\x -> case x of { ("enter":room:_) -> Just room; (_) -> Nothing })
    , (\x -> case x of { ("go":"into":room:_) -> Just room; (_) -> Nothing })
    , (\x -> case x of { ("pick":"up":item:_) -> Just item; (_) -> Nothing })
    , (\x -> case x of { ("take":item:_) -> Just item; (_) -> Nothing })
    ]

-- parse :: String -> (a -> a, b -> b, c -> c, String)
    
