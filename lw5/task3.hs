import System.Environment (getArgs)
import System.IO
import System.Directory (canonicalizePath, renameFile, removeFile)
import System.FilePath (takeDirectory)
import System.Exit (die)
import Control.Exception (IOException, try, onException)
import Data.Char (isDigit)
import Text.Read (readMaybe)

viewFile :: FilePath -> IO ()
viewFile file = withFile file ReadMode (\h -> do
    text <- hGetContents h
    putStr text)

addLine :: FilePath -> IO ()
addLine file = do
    putStrLn "Enter new line:"
    line <- getLine
    hasBreak <- withBinaryFile file ReadMode (\h -> do
        size <- hFileSize h
        if size == 0
            then return True
            else do
                hSeek h SeekFromEnd (-1) -- перемещает указатель в конец файла
                c <- hGetChar h          --читаем последний байт файла
                return (c == '\n'))

    let separator = if hasBreak then "" else "\n"
    appendFile file (separator ++ line ++ "\n")
    putStrLn "Line added."

copyExcept :: Handle -> Handle -> Int -> IO Bool
copyExcept source target number = go 1 False
    where
        go current found = do
            end <- hIsEOF source

            if end
                then return found
                else do
                    line <- hGetLine source

                    if current /= number
                        then hPutStrLn target line
                        else return ()

                    go (current + 1) (found || current == number)

deleteLine :: FilePath -> IO ()
deleteLine file = do
    putStrLn "Enter line number:"
    input <- getLine

    case readMaybe input of
        Just n | n > 0 -> do
            (temp, h) <- openTempFile (takeDirectory file) "task3.tmp"
            hClose h

            found <- (withFile file ReadMode (\source ->
                withFile temp WriteMode (\target ->
                    copyExcept source target n))) `onException` removeFile temp

            if found
                then do
                    renameFile temp file `onException` removeFile temp
                    putStrLn "Line deleted."
                else do
                    removeFile temp
                    die "Error: line not found."

        _ -> die "Error: invalid line number."

copyFiltered :: FilePath -> FilePath -> (Char -> Bool) -> IO ()
copyFiltered input output selectedFilter = do
    a <- canonicalizePath input
    b <- canonicalizePath output

    if a == b
        then die "Error: files must be different."
        else do
            withFile input ReadMode (\source ->
                withFile output WriteMode (\target -> do
                    text <- hGetContents source
                    hPutStr target (filter selectedFilter text)))
            putStrLn "File copied."

keepNonDigit :: Char -> Bool
keepNonDigit c = not (isDigit c)

keepOther :: Char -> Char -> Bool
keepOther symbol c = c /= symbol

filterFile :: FilePath -> FilePath -> IO ()
filterFile input output = do
    putStrLn "1 - Remove digits"
    putStrLn "2 - Remove character"
    choice <- getLine

    case choice of
        "1" -> copyFiltered input output keepNonDigit
        "2" -> do
            putStrLn "Enter character:"
            text <- getLine
            case text of
                [c] -> copyFiltered input output (keepOther c)
                _ -> die "Error: enter one character."
        _ -> die "Error: invalid filter."

run :: IO ()
run = do
    args <- getArgs

    case args of
        [input, output] -> do
            putStrLn "1 - View"
            putStrLn "2 - Add line"
            putStrLn "3 - Delete line"
            putStrLn "4 - Copy with filter"
            choice <- getLine

            case choice of
                "1" -> viewFile input
                "2" -> addLine input
                "3" -> deleteLine input
                "4" -> filterFile input output
                _ -> die "Error: invalid action."

        _ -> die "Usage: runghc Task3.hs input.txt output.txt"

main :: IO ()
main = do
    result <- try run :: IO (Either IOException ())

    case result of
        Left err -> die ("Error: " ++ show err)
        Right _ -> return ()