module Main where

import           Data.Char
import           Control.Monad

main :: IO ()

main = do
    sayHello
    reverser

sayHello = do
    putStrLn "What's your first name?"
    firstName <- getLine
    putStrLn "What's your last name?"
    lastName <- getLine
    let bigFirstName = map toUpper firstName
        bigLastName  = map toUpper lastName
    putStrLn $ "hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?"

reverser = do
    putStrLn "Write words to reverse, empty to move on."
    line <- getLine
    if null line
        then return ()
        else do
            putStrLn $ reverseWords line
            reverser

reverseWords :: String -> String
reverseWords = unwords . map reverse . words

useMapM = do
    as <- mapM print [1, 2, 3]
    mapM_ print [4, 5, 6]
