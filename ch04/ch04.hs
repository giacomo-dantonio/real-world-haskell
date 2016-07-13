import Data.List (foldl')
import Data.Char (digitToInt, isDigit)


-- Exercises on page 84

-- Exercise 1

-- if you use condom it's safe
condom :: ([a] -> b) -> [a] -> Maybe b
condom _ [] = Nothing
condom unsafe as = Just (unsafe as)

safeHead :: [a] -> Maybe a
safeHead = condom head

safeTail :: [a] -> Maybe [a]
safeTail = condom tail

safeLast :: [a] -> Maybe a
safeLast = condom last

safeInit :: [a] -> Maybe [a]
safeInit = condom init


-- Exercise 2

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = [[]]
splitWith _ [a] = [[a]]
splitWith predicate (a:as) = if predicate a
        then (a : head rest) : tail rest
        else [a] : rest
        where
            rest = splitWith predicate as


-- Exercises on page 97

-- Exercise 1 and 2 and 3

asIntErrorMessage = "Invalid Format, not a number"

asInt :: String -> Int
asInt "" = error asIntErrorMessage
asInt "-" = error asIntErrorMessage
asInt ('-':xs) = (-asInt xs)
asInt xs = foldl' loop 0 xs
    where
        loop acc i
            | digit i > maxBound - 10 * acc = error "Int overflow"
            | otherwise = 10 * acc + digit i
        digit i
            | isDigit i = digitToInt i
            | otherwise = error asIntErrorMessage


-- Exercise 4

type ErrorMessage = String

asInt_either :: String -> Either Int ErrorMessage
asInt_either "" = Right asIntErrorMessage
asInt_either "-" = Right asIntErrorMessage
asInt_either ('-':xs) = opposite (asInt_either xs)
    where
        opposite :: Either Int ErrorMessage -> Either Int ErrorMessage
        opposite (Left n) = Left (-n)
        opposite rightMessage = rightMessage
asInt_either xs = foldl' loop (Left 0) xs
    where
        loop (Left acc) i
            | not (isDigit i) = error asIntErrorMessage
            | digitToInt i > maxBound - 10 * acc = Right "Int overflow"
            | otherwise = Left (10 * acc + digitToInt i)
        loop rightMessage _ = rightMessage


-- Exercise 6

concat_fold :: [[a]] -> [a]
concat_fold = foldr (++) []


-- Exercise 7

takeWhile_recursive :: (a -> Bool) -> [a] -> [a]
takeWhile_recursive _ [] = []
takeWhile_recursive predicate (x:xs)
    | predicate x = x : (takeWhile_recursive predicate xs)
    | otherwise = []

takeWhile_foldr :: (a -> Bool) -> [a] -> [a]
takeWhile_foldr predicate = foldr pick []
    where
        pick x acc
            | (predicate x) = x : acc
            | otherwise = []


-- Exercise 9

groupBy_fold :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy_fold equals = foldr loop []
    where
        loop x acc
            | null acc = [[x]]
            | null (head acc) = [x] : (tail acc)
            | x `equals` head (head acc) = (x : head acc) : (tail acc)
            | otherwise = [x] : acc


-- Exercise 10

any_fold :: (a -> Bool) -> [a] -> Bool
any_fold predicate = foldr (\x acc -> predicate x || acc) False

-- about cycle: you can't produce an infinite list using only cicle,
-- you can do something like ``foldr (++) [] (repeat xs)`` but isn't repeat
-- essentialy the same as cycle?
-- So I don't think it makes sense to implement cycle as a fold
