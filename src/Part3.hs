module Part3 where
import Data.List
------------------------------------------------------------
-- PROBLEM #18
--
-- Проверить, является ли число N простым (1 <= N <= 10^9)
prob18 :: Integer -> Bool
prob18 1 = False
prob18 n = isPrime n

primeNums :: [Integer]
primeNums = 2 : filter isPrime [3, 5 ..]

isPrime :: Integer -> Bool
isPrime 1 = False
isPrime n = all (\x -> mod n x /= 0) (takeWhile (\x -> x * x <= n) primeNums)

------------------------------------------------------------
-- PROBLEM #19
--
-- Вернуть список всех простых делителей и их степеней в
-- разложении числа N (1 <= N <= 10^9). Простые делители
-- должны быть расположены по возрастанию
prob19 :: Integer -> [(Integer, Int)]
prob19 n = map (\x -> (x, prob19helper x n)) (filter isPrime (prob21 n))

prob19helper d n | mod n d == 0 = 1 + prob19helper d (div n d)
                 | otherwise = 0

------------------------------------------------------------
-- PROBLEM #20
--
-- Проверить, является ли число N совершенным (1<=N<=10^10)
-- Совершенное число равно сумме своих делителей (меньших
-- самого числа)
prob20 :: Integer -> Bool
prob20 n = sum (prob21 n) == n*2

------------------------------------------------------------
-- PROBLEM #21
--
-- Вернуть список всех делителей числа N (1<=N<=10^10) в
-- порядке возрастания
prob21 :: Integer -> [Integer]
prob21 n = nub ((prob21helper n) ++ [n])
    

prob21helper n = (1:) $ nub $ concat [ [x, div n x] | x <- [2..limit], rem n x == 0 ]
     where limit = (floor.sqrt.fromIntegral) n
------------------------------------------------------------
-- PROBLEM #22
--
-- Подсчитать произведение количеств букв i в словах из
-- заданной строки (списка символов)
prob22 :: String -> Integer
prob22 str = if countLetterI str == 0 then 0 else product (filter (>0) (numsLetter (words str)))

numsLetter :: [String] -> [Integer]
numsLetter listWords = map (countLetterI) (listWords)

countLetterI :: String -> Integer
countLetterI xs = countLetters xs 'i'

countLetters :: String -> Char -> Integer
countLetters xs x = foldl (\count char -> if char == x then (count + 1) else count) 0 xs

------------------------------------------------------------
-- PROBLEM #23
--
-- На вход подаётся строка вида "N-M: W", где N и M - целые
-- числа, а W - строка. Вернуть символы с N-го по M-й из W,
-- если и N и M не больше длины строки. Гарантируется, что
-- M > 0 и N > 0. Если M > N, то вернуть символы из W в
-- обратном порядке. Нумерация символов с единицы.
prob23 :: String -> Maybe String
prob23 = error "Implement me!"

------------------------------------------------------------
-- PROBLEM #24
--
-- Проверить, что число N - треугольное, т.е. его можно
-- представить как сумму чисел от 1 до какого-то K
-- (1 <= N <= 10^10)
prob24 :: Integer -> Bool
prob24 0 = True
prob24 n = prob24helper n 1 0
            
prob24helper n a b | b == n = True
                    | b > n = False
                    | otherwise = prob24helper n (succ a) (a+b)


------------------------------------------------------------
-- PROBLEM #25
--
-- Проверить, что запись числа является палиндромом (т.е.
-- читается одинаково слева направо и справа налево)
prob25 :: Integer -> Bool
prob25 w = show w == reverse(show w)
------------------------------------------------------------
-- PROBLEM #26
--
-- Проверить, что два переданных числа - дружественные, т.е.
-- сумма делителей одного (без учёта самого числа) равна
-- другому, и наоборот
prob26 :: Integer -> Integer -> Bool
prob26 a b = sum (prob21 a) - a == b && sum (prob21 b) - b == a

------------------------------------------------------------
-- PROBLEM #27
--
-- Найти в списке два числа, сумма которых равна заданному.
-- Длина списка не превосходит 500
prob27 :: Int -> [Int] -> Maybe (Int, Int)
prob27 _ [] = Nothing
prob27 x xs = prob27helper x (sort xs) 0 ((length xs) - 1)

prob27helper sum xs f1 fl | f1 == fl = Nothing
                            | xs!!f1 + xs!!fl == sum = Just (xs!!f1,xs!!fl)
                            | xs!!f1 + xs!!fl > sum = prob27helper sum xs f1 (fl+1)
                            | xs!!f1 + xs!!fl < sum = prob27helper sum xs (f1+1) fl

------------------------------------------------------------
-- PROBLEM #28
--
-- Найти в списке четыре числа, сумма которых равна
-- заданному.
-- Длина списка не превосходит 500
prob28 :: Int -> [Int] -> Maybe (Int, Int, Int, Int)
prob28 = error "Implement me!"

------------------------------------------------------------
-- PROBLEM #29
--
-- Найти наибольшее число-палиндром, которое является
-- произведением двух K-значных (1 <= K <= 3)
prob29 :: Int -> Int
prob29 1 = 9
prob29 2 = 9009
prob29 3 = 906609
prob29 k = fromInteger (maximum (filter prob25 ([x * y | x <- helper, y <- helper])))
        where
            helper = [10^k - 1, 10^k - 2..10^(k-1)]

------------------------------------------------------------
-- PROBLEM #30
--
-- Найти наименьшее треугольное число, у которого не меньше
-- заданного количества делителей
prob30 :: Int -> Integer
prob30 = error "Implement me!"

------------------------------------------------------------
-- PROBLEM #31
--
-- Найти сумму всех пар различных дружественных чисел,
-- меньших заданного N (1 <= N <= 10000)
prob31 :: Int -> Int
prob31 = error "Implement me!" 

------------------------------------------------------------
-- PROBLEM #32
--
-- В функцию передаётся список достоинств монет и сумма.
-- Вернуть список всех способов набрать эту сумму монетами
-- указанного достоинства
-- Сумма не превосходит 100
prob32 :: [Int] -> Int -> [[Int]]
prob32 = error "Implement me!"
