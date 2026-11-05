import System.Environment (getArgs)
import System.IO
import System.Directory (canonicalizePath)
import System.Exit (die)
import Control.Exception (IOException, try)
import Data.Char (isPunctuation)

replaceChar :: Char -> Char -> Char
replaceChar symbol c
    | isPunctuation c = symbol
    | otherwise = c

sameFile :: FilePath -> FilePath -> IO Bool
sameFile input output = do
    path1 <- canonicalizePath input
    path2 <- canonicalizePath output
    return (path1 == path2)

copyText :: FilePath -> FilePath -> Char -> IO ()
copyText input output symbol = do
    same <- sameFile input output

    if same
        then die "Error: files must be different."
        else do
            withFile input ReadMode (\source ->
                withFile output WriteMode (\target -> do
                    content <- hGetContents source
                    hPutStr target (map (replaceChar symbol) content)))

            putStrLn "File copied successfully."

run :: IO ()
run = do
    args <- getArgs

    case args of
        [input, output] -> do
            putStrLn "Enter replacement character:"
            text <- getLine

            case text of
                [symbol] -> copyText input output symbol
                _ -> die "Error: enter exactly one character."

        _ -> die "Usage: runghc Task2.hs input.txt output.txt"

main :: IO ()
main = do
    result <- try run :: IO (Either IOException ())

    case result of
        Left err -> die ("Error: " ++ show err)
        Right _ -> return ()
