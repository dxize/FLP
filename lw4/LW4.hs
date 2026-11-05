module LW4
  ( myIntersperse
  , myIsPrefixOf
  , myIsDigit
  , myMapSize
  , mySetMember
  , myIsDigit2
  , myMapSize2
  , mySetMember2
  ) where

import qualified Data.Map as Map
import qualified Data.Set as Set


--            intersperse (Data.List)

-- Тип: a -> [a] -> [a]
-- Назначение: вставляет указанный элемент
-- между соседними элементами списка

-- Примеры:
-- intersperse 0 [1,2,3]  -> [1,0,2,0,3]
-- intersperse '-' "abc"  -> "a-b-c"
-- intersperse 0 []       -> []

myIntersperse :: a -> [a] -> [a]
myIntersperse _ [] = []
myIntersperse _ [x] = [x]
myIntersperse sep (x:xs) = x : sep : myIntersperse sep xs


--            isPrefixOf (Data.List)

-- Тип: Eq a => [a] -> [a] -> Bool
-- Назначение: проверяет, является ли первый
-- список началом второго списка.

-- Примеры:
-- isPrefixOf "ab" "abcd"  -> True
-- isPrefixOf "xy" "abcd"  -> False
-- isPrefixOf "" "hello"   -> True

myIsPrefixOf :: Eq a => [a] -> [a] -> Bool
myIsPrefixOf [] _ = True
myIsPrefixOf _ [] = False
myIsPrefixOf (x:xs) (y:ys) = x == y && myIsPrefixOf xs ys


--            isDigit (Data.Char)

-- Тип: Char -> Bool
-- Назначение: проверяет, является ли символ
-- цифрой от '0' до '9'.

-- Примеры:
-- isDigit '5' -> True
-- isDigit 'A' -> False
-- isDigit ' ' -> False

myIsDigit :: Char -> Bool
myIsDigit c = c >= '0' && c <= '9'


--            size (Data.Map)

-- Тип: Map.Map k v -> Int
-- Назначение: возвращает количество пар
-- ключ-значение в словаре Map.

-- Примеры:
-- Map.size (Map.fromList [(1,10),(2,20)]) -> 2
-- Map.size Map.empty                      -> 0
-- Map.size (Map.fromList [(1,10),(1,20)]) -> 1

myMapSize :: Map.Map k v -> Int
myMapSize m = length (Map.toList m)



--            member (Data.Set)

-- Тип: Ord a => a -> Set.Set a -> Bool
-- Назначение: проверяет наличие элемента
-- во множестве Set.

-- Примеры:
-- Set.member 2 (Set.fromList [1,2,3]) -> True
-- Set.member 5 (Set.fromList [1,2,3]) -> False
-- Set.member 1 Set.empty              -> False

mySetMember :: Ord a => a -> Set.Set a -> Bool
mySetMember x s = elem x (Set.toList s)



-- ДОПОЛНИТЕЛЬНОЕ ЗАДАНИЕ
-- Альтернативные реализации

-- isDigit через elem
myIsDigit2 :: Char -> Bool
myIsDigit2 c = elem c ['0'..'9']

-- Map.size через foldr
myMapSize2 :: Map.Map k v -> Int
myMapSize2 m = foldr (\_ n -> n + 1) 0 (Map.toList m)

-- Set.member через any
mySetMember2 :: Ord a => a -> Set.Set a -> Bool
mySetMember2 x s = any (== x) (Set.toList s)


main :: IO ()
main = do

  putStrLn "------myIntersperse------"
  print (myIntersperse 0 [1,2,3])
  print (myIntersperse '-' "abc")
  print (myIntersperse 0 ([] :: [Int]))

  putStrLn "------myIsPrefixOf------"
  print (myIsPrefixOf "ab" "abcd")
  print (myIsPrefixOf "xy" "abcd")
  print (myIsPrefixOf "" "hello")

  putStrLn "------myIsDigit------"
  print (myIsDigit '5')
  print (myIsDigit 'A')
  print (myIsDigit ' ')

  putStrLn "------myMapSize------"
  print (myMapSize (Map.fromList [(1,10),(2,20)]))
  print (myMapSize (Map.empty :: Map.Map Int Int))
  print (myMapSize (Map.fromList [(1,10),(1,20)]))

  putStrLn "------mySetMember------"
  print (mySetMember 2 (Set.fromList [1,2,3]))
  print (mySetMember 5 (Set.fromList [1,2,3]))
  print (mySetMember 1 (Set.empty :: Set.Set Int))

  putStrLn "------alternative implementations------"
  print (myIsDigit2 '5')
  print (myMapSize2 (Map.fromList [(1,10),(2,20)]))
  print (mySetMember2 2 (Set.fromList [1,2,3]))