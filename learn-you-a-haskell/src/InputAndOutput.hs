-- Chapter 9: http://learnyouahaskell.com/input-and-output
-- Enter interactive mode with `stack ghci`
-- and load the module with `:l StartingOut`
module InputAndOutput
    ()
where

import           Control.Monad
import           Data.Char
import           System.IO

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

-- Files and streams

-- getContents is an I/O actions that reads everything from the standard input until it encounters EOF. But it does it lazily! It does not read the contents with `foo <- getContents` but when it's actually accessed.

contentReader = do
    contents <- getContents
    putStr (map toUpper contents)

shortLineAcceptor = do
    contents <- getContents
    putStr $ shortLinesOnly contents

shortLinesOnly :: String -> String
shortLinesOnly input =
    let allLines   = lines input
        shortLines = filter (\line -> length line < 10) allLines
        result     = unlines shortLines
    in  result

-- `interact` is made for the pattern of getting some string from the input, transforming it and then outputting it. It takes a function `String -> String` and returns an I/O action that takes inputs, runs the function on it, and prints the result. Observe:

shortLinesAcceptorWithInteract = interact shortLinesOnly

-- Another example:
respondPalidromes =
    unlines
        . map
              (\xs ->
                  if isPalindrome xs then "palindrome" else "not a palindrome"
              )
        . lines
    where isPalindrome xs = xs == reverse xs

respondPalidromesMain = interact respondPalidromes

-- So far we've been reading from standard input and writing to standard output. Writing files is very similar. Here's a program that reads a file:

fileReader = do
    handle   <- openFile "girlfriend.txt" ReadMode
    contents <- hGetContents handle
    putStr contents
    hClose handle

-- Note the difference between the handle used to identify a file and the contents of the file, bound in our program to handle and contents. The handle is just something by which we know what our file is. If you imagine your whole file system to be a really big book and each file is a chapter in the book, the handle is a bookmark that shows where you're currently reading (or writing) a chapter, whereas the contents are the actual chapter.

-- Here's the same with `withFile`:
withFileExample = withFile
    "girlfriend.txt"
    ReadMode
    (\handle -> do
        contents <- hGetContents handle
        putStr contents
    )

-- withFile essentially does this:
withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile' path mode f = do
    handle <- openFile path mode
    result <- f handle
    hClose handle
    return result

-- Just like we have hGetContents that works like getContents but for a specific file, there's also hGetLine, hPutStr, hPutStrLn, hGetChar, etc. They work just like their counterparts without the h, only they take a handle as a parameter and operate on that specific file instead of operating on standard input or standard output. 

-- readFile has a type signature of readFile :: FilePath -> IO String. Remember, FilePath is just a fancy name for String. readFile takes a path to a file and returns an I/O action that will read that file (lazily, of course) and bind its contents to something as a string.

readFileExample = do
    contents <- readFile "girlfriend.txt"
    putStr contents

-- writeFile has a type of writeFile :: FilePath -> String -> IO (). It takes a path to a file and a string to write to that file and returns an I/O action that will do the writing.

writeFileExample = do
    contents <- readFile "girlfriend.txt"
    let transformedText = map toUpper contents
    writeFile "girlfriendcaps.txt" transformedText

-- appendFile has a type signature that's just like writeFile, only appendFile doesn't truncate the file to zero length if it already exists but it appends stuff to it.

appendExample = do
    todoItem <- getLine
    appendFile "todo.txt" (todoItem ++ "\n")

-- Just like you can think of lists as streams, you can also think of files as streams. This will read one line at a time and print it out to the terminal as it goes along. So you may be asking, how wide is this pipe then? How often will the disk be accessed? Well, for text files, the default buffering is line-buffering usually. That means that the smallest part of the file to be read at once is one line. That's why in this case it actually reads a line, prints it to the output, reads the next line, prints it, etc. For binary files, the default buffering is usually block-buffering. That means that it will read the file chunk by chunk. The chunk size is some size that your operating system thinks is cool.

-- You can control how exactly buffering is done by using the hSetBuffering function.
manualBufferingExample = withFile
    "something.txt"
    ReadMode
    (\handle -> do
        hSetBuffering handle $ BlockBuffering (Just 2048)
        contents <- hGetContents handle
        putStr contents
    )
