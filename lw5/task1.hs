import Text.Read (readMaybe)
import System.Exit (exitFailure)

readNumber :: String -> IO Int
readNumber message = do
    putStrLn message
    input <- getLine

    case readMaybe input of
        Just number -> return number
        Nothing -> do
            putStrLn "Error: enter an integer."
            exitFailure

findFirst :: Int -> Int -> Int
findFirst start k
    | start `mod` k == 0 = start
    | otherwise = (start `div` k + 1) * k

createList :: Int -> Int -> Int -> [Int]
createList start count k =
    let first = findFirst start k
    in take count [first, first + k ..]

printList :: Int -> Int -> Int -> IO ()
printList start count k
    | count < 0 = putStrLn "Error: count cannot be negative."
    | k <= 0 = putStrLn "Error: multiple must be positive."
    | otherwise = print (createList start count k)

main :: IO ()
main = do
    start <- readNumber "Enter start number:"
    count <- readNumber "Enter element count:"
    k <- readNumber "Enter multiple:"

    printList start count k