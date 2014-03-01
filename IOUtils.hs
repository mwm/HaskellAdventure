module IOUtils
( prompt
, promptTest
, promptNotTest
) where

import System.IO (hFlush, stdout)

prompt :: String -> IO String
prompt put = do
    putStr put
    hFlush stdout
    getLine

promptTest :: [String -> Bool] -> String -> IO String
promptTest tests put = do
    val <- prompt put

    if True `elem` (map ($ val) tests)
         then promptTest tests put
         else return val

promptNotTest :: [String -> Bool] -> String -> IO String
promptNotTest tests put = do
    promptTest (map (not .) tests) put

