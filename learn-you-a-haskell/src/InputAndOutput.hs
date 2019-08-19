-- Chapter 9: http://learnyouahaskell.com/input-and-output
-- Enter interactive mode with `stack ghci`
-- and load the module with `:l StartingOut`
module InputAndOutput
    ()
where


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

