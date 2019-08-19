-- Chapter 9: http://learnyouahaskell.com/input-and-output
-- Enter interactive mode with `stack ghci`
-- and load the module with `:l StartingOut`
module InputAndOutput
    ()
where

import           Control.Monad
import           Data.Char

-- Pure functions cannot change state of things in the world. To achieve effects,
-- Haskell uses the IO type.

-- For example, the type of `putStrLn`:
-- ghci> :t putStrLn  
-- putStrLn :: String -> IO ()  
-- So the function takes a string and returns an I/O action.

-- Here's how you compose I/O actions in the main program with `do`:

-- main = do
--    putStrLn "Hello, what's your name?"
--   name <- getLine
--    putStrLn ("Hey " ++ name ++ ", you rock!")

-- What's the type of getLine?
-- ghci> :t getLine  
-- getLine :: IO String

-- getLine is an I/O action that contains a result of type String. Once the result is available, the only way to open the I/O box and get the data inside it is to use the <- construct. Taking data out of an I/O action can only be done when inside another I/O action. This is how Haskell separates the pure and impure parts of the code.

-- Let bindings can be used in `do` blocks without the `in` part to bind pure expressions to names:

-- main = do  
--    putStrLn "What's your first name?"  
--    firstName <- getLine  
--    putStrLn "What's your last name?"  
--    lastName <- getLine  
--    let bigFirstName = map toUpper firstName  
--        bigLastName = map toUpper lastName  
--    putStrLn $ "hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?"  

-- In Haskell (in I/O actions specifically), "return" makes an I/O action out of a pure value. If you think about the box analogy from before, it takes a value and wraps it up in a box. The resulting I/O action doesn't actually do anything, it just has that value encapsulated as its result.

useReturn = do
    a <- return "hell"
    b <- return "yeah!"
    putStrLn $ a ++ " " ++ b

-- When dealing with I/O do blocks, we mostly use return either because we need to create an I/O action that doesn't do anything or because we don't want the I/O action that's made up from a do block to have the result value of its last action, but we want it to have a different result value, so we use return to make an I/O action that always has our desired result contained and we put it at the end.

-- "when" takes a boolean value and an I/O action if that boolean value is True, it returns the same I/O action that we supplied to it. However, if it's False, it returns the return (), action, so an I/O action that doesn't do anything. It's useful for encapsulating the if something then do some I/O action else return () pattern.
useWhen = do
    putStrLn "Hello, give a char"
    c <- getChar
    when (c /= ' ') $ do
        putChar c
        useWhen

-- sequence takes a list of I/O actions and returns an I/O actions that will perform those actions one after the other.
useSequence = do
    rs <- sequence [getLine, getLine, getLine]
    print rs

-- Because mapping a function that returns an I/O action over a list and then sequencing it is so common, the utility functions mapM and mapM_ were introduced. mapM takes a function and a list, maps the function over the list and then sequences it. mapM_ does the same, only it throws away the result later. We usually use mapM_ when we don't care what result our sequenced I/O actions have.
useMapM = do
    as <- mapM print [1, 2, 3]
    mapM_ print [4, 5, 6]

-- forever takes an I/O action and returns an I/O action that just repeats the I/O action it got forever.
main = forever $ do
    putStr "Give me some input: "
    l <- getLine
    putStrLn $ map toUpper l

-- forM (located in Control.Monad) is like mapM, only that it has its parameters switched around. The first parameter is the list and the second one is the function to map over that list, which is then sequenced. Why is that useful? Well, with some creative use of lambdas and do notation, we can do stuff like this:
useForM = do
    colors <- forM
        [1, 2, 3, 4]
        (\a -> do
            putStrLn
                $  "Which color do you associate with the number "
                ++ show a
                ++ "?"
            color <- getLine
            return color
        )
    putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "
    mapM putStrLn colors
-- You can think of forM as meaning: make an I/O action for every element in this list.

