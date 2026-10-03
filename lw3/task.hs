listNums :: Int -> [Int]
listNums n
    | n > 1 = n : listNums (n - 1)
    | n < 1 = n : listNums (n + 1)
    | otherwise = [1]

secondLastList :: [[any]] -> [any]
secondLastList [] = []
secondLastList ([]:xs) = secondLastList xs
secondLastList (x:xs) = last x : secondLastList xs

myUnion :: Eq any => [any] -> [any] -> [any]
myUnion [] [] = []
myUnion (x:xs) l2
    | elem x xs = myUnion xs l2
    | elem x l2 = myUnion xs l2
    | otherwise = x : myUnion xs l2
myUnion [] (y:ys)
    | elem y ys = myUnion [] ys
    | otherwise = y : myUnion [] ys

mySubst :: Eq any => [any] -> [any] -> [any]
mySubst [] l2 = []
mySubst (x:xs) l2
    | elem x l2 = mySubst xs l2
    | otherwise = x : mySubst xs l2

nposList :: [[a]] -> Int -> [a]
nposList l n
    | n < 1 = error "Index cannot be negative"
    | any (\x -> x < n)(map length l) = error "Attempt to retrieve a non-existent element"
    | otherwise = map (!! (n - 1)) l

main  = do
    -- блок проверок для первой функции
    print (listNums 12)
    print (listNums 5)
    print (listNums (-10))
    print (listNums 0)
    -- блок проверок для второй функции
    print (secondLastList ([[123, 123123, 1 ], [12, 3], [], [1], []]))
    print (secondLastList ([['d', 's', '1' ]]))
    print (secondLastList [] :: [[Int]])
    -- блок проверок для третьей функции
    print(myUnion ([] :: [Int]) [])
    print(myUnion [-1, 2, 4] [3, 5, 9])
    print(myUnion [1, 1, 3] [1, 4, 5])
    print(myUnion [1, 4] [4, 5, 6])
    print(myUnion [1, 4, 5, 6] [4, 5])
    print(myUnion [1, 4, 5, 6] [])
    -- блок проверок для четвёртой функции
    print (mySubst [1,2,3,4] [3,4,5])
    print (mySubst [1,1,2,3,3] [3,4])
    print (mySubst [1,2,3] [])
    print (mySubst [] [1,2,3])
    print (mySubst [1,2,3] [1,2,3])
    print (mySubst [1,2,3,4,5] [2,4])
    -- блок проверок для пятой функции
    print (nposList [[1,2,3], [4,5,6], [7,8,9]] 1)
    print (nposList [[1,2,3], [4,5,6], [7,8,9]] 2)
    print (nposList [[1,2,3], [4,5,6], [7,8,9]] 3)
    print (nposList ["abc", "xyz", "qwe"] 2)
    print (nposList ([] :: [[Int]]) 1)
    --print (nposList [[1,2], [3,4,5]] 3)
    --print (nposList [[1,2,3]] (-1))
