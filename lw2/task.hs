module Lw2 where

do_my_list :: Int -> [Int]
do_my_list n = take n [n..]

oddEven :: [any] -> [any]
oddEven [] = []
oddEven [x] = [x]
oddEven (x:y:xs) = y : x : oddEven xs

insert :: ([any], any, Int) -> [any]
insert ([], a, _) = [a]
insert (xs, a, n) | n <= 0 = a : xs
insert (x:xs, a, n) = x : insert (xs, a, n - 1)

listSumm :: Num any => ([any], [any]) -> [any]
listSumm (l1, []) = l1
listSumm ([], l2) = l2
listSumm (x:xs, y:ys) = (x + y) : listSumm(xs, ys)

position :: Eq any => ([any], any) -> Int
position ([], _) = -1
position (x:xs, a)
    | x == a = 0
    | position (xs, a) == -1 = -1
    | otherwise = position (xs, a) + 1

firstSumFunction :: Int -> Int
firstSumFunction n | n < 1 = -1
firstSumFunction n | n == 1 = 1
firstSumFunction n = firstSumFunction (n - 1) + n

secondSumFunction :: Int -> Int
secondSumFunction n | n < 1 = -1
secondSumFunction n | n == 1 = 0
secondSumFunction n = secondSumFunction (n - 1) + (n - 1)

main = do
    n <- readLn :: IO Int
    let list1 = do_my_list n
    print list1
    print (oddEven list1)
    print (insert (list1, 312, 2))
    n <- readLn :: IO Int
    let list2 = do_my_list n
    print list2
    print (listSumm (list1, list2))
    print (position (list2, 8))
    print (firstSumFunction 5)
    print (secondSumFunction 5)

