{-# LANGUAGE ExistentialQuantification #-}

-- File: rdm.hs
-- Date: 02/10/2010
-- Author: Collin J. Doering <rekahsoft@gmail.com>
-- Description: Random source file to experiment while learning haskell

import System.IO
import Data.List
import Data.Foldable (foldr')
import Data.Function
import System.Posix.User
import Control.Monad

-- Given a item and a list finds the index's (if any) where the item exist in the list
fp :: a -> [a] -> [Int]
fp y xs = fst $ fp' xs ([],0)
  where fp' [] acc = acc
        fp' (x:xs) (zs,i)
          | x == y = fp' xs (i:zs, i + 1)
          | otherwise = fp' xs (zs, i + 1)

betterFp :: a -> [a] -> [Int]
betterFp y = fst . foldr (\a (xs,i) -> if y == a then (i:xs,i+1) else (xs,i+1)) ([],0)

nameSpam :: IO ()
nameSpam s = putStrLn $ fst $ foldr (\x (a, i) -> (take i (repeat x) ++ a, i - 1)) ("", length s) s

printTriangle :: Char -> Int -> IO ()
printTriangle c i = pTriangle c 1
  where pTriangle c j
          | j > i = return ()
          | otherwise = putStrLn (take j (repeat c)) >>
                        pTriangle c (j + 1)

printTriangle' :: Char -> Int -> IO ()
printTriangle' _ 0 = return ()
printTriangle' c i = putStrLn (take i (repeat c)) >> printTriangle' c (i - 1)

printTriangle'' :: Char -> Integer -> IO ()
printTriangle'' c n = putStrLn $ foldr' (\i a -> (take i $ repeat c) ++ "\n" ++ a) "" [1..n]

factorial :: Integer -> Integer
factorial x = if x <= 1 then 1
              else x * factorial (x - 1)

-- The factorial function using fix points
factorial' = fix (\f x -> if x <= 1 then 1 else x * f(x - 1))

factorial'' = fix (\f acc x -> if x <= 1 then acc else f (acc * x) (x - 1)) 1

factorial1 :: Integer -> Integer
factorial1 0 = 1
factorial1 xs = xs * factorial1 (xs - 1)

squareList :: [Double] -> [Double]
squareList lst = if null lst then []
                 else (square (head lst)):(squareList (tail lst))
                      where square x = x * x

squareList1 :: [Double] -> [Double]
squareList1 [] = []
squareList1 (x:xs) = (square x):(squareList1 xs)
                     where square x = x * x

squareList2 = map (\x -> x * x)

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib x = fib (x-1) + fib (x-2)

-- Playing with datatypes
data Posn = Posn2D { x :: Int, y :: Int }
          | Posn3D { x :: Int, y :: Int, z :: Int }
          deriving (Show, Eq, Ord)

-- Real World Haskell Exercises
data List a = Cons a (List a)
            | Nil
              deriving (Show)

listToBIL :: List a -> [a]
listToBIL (Cons a xs) = a:(listToBIL xs)
listToBIL Nil = []

myLength :: [a] -> Integer
myLength [] = 0
myLength x = 1 + myLength (drop 1 x)

myLength1 :: [a] -> Integer
myLength1 lst = let myLength1Help [] acc = acc
                    myLength1Help (_:xs) acc = myLength1Help xs (acc + 1)
                in myLength1Help lst 0

myLength2 :: [a] -> Int
myLength2 lst = myLength2' lst 0
  where myLength2' [] a = a
        myLength2' (_:xs) a = myLength2' xs (a + 1)

meanOfList :: [Double] -> Double
meanOfList lst = meanSum lst 0 0
  where meanSum [] s l
          | l /= 0 = s / l
          | otherwise = 0
        meanSum (x:xs) s l = meanSum xs (s + x) (l + 1)

listToPalindrome :: [a] -> [a]
listToPalindrome [] = []
listToPalindrome x = x ++ reverse x

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x
  | mod len 2 == 0 && take (div len 2) x == reverse (drop (div len 2) x) = True
  | otherwise                                                            = False
  where len = length x

foldrmap :: (a -> b) -> [a] -> [b]
foldrmap fn = foldr (\x y -> (fn x):y) []
--foldrmap fn = foldr ((:) . fn) []

foldrcopy :: [a] -> [a]
foldrcopy = foldr (:) []

foldrappend :: [a] -> [a] -> [a]
foldrappend a b = foldr (:) b a

foldrlength :: [a] -> Int
foldrlength = foldr (\x y -> y + 1) 0

foldrsum :: (Num a) => [a] -> a
foldrsum = foldr (+) 0

--myfoldr fn init lst = myFoldrHelper ...

myMap :: (a -> b) -> [a] -> [b]
myMap f xs = [f x | x <- xs]

myMap1 :: (a -> b) -> [a] -> [b]
myMap1 _ [] = []
myMap1 f (x:xs) = f x : myMap1 f xs

mapWithFilter :: (a -> b) -> (a -> Bool) -> [a] -> [b]
mapWithFilter f p xs = [f x | x <- xs, p x]

mapWithFilter1 :: (a -> b) -> (a -> Bool) -> [a] -> [b]
mapWithFilter1 _ _ [] = []
mapWithFilter1 f p (x:xs)
  | p x = f x : mapWithFilter1 f p xs
  | otherwise = mapWithFilter1 f p xs

mapWithFilter2 :: (a -> b) -> (a -> Bool) -> [a] -> [b]
mapWithFilter2 f p = map f . filter p

-- A neat little closure
myFlip :: (a -> b -> c) -> b -> a -> c
myFlip f = \a b -> f b a

compose :: (a -> b) -> (c -> a) -> (c -> b)
compose f g = \x -> f(g(x))

disemvowel :: String -> String
disemvowel = unwords . filter p . words
  where p = flip elem "AaEeIiOoUu" . head

-- questions from http://www.haskell.org/haskellwiki/Hitchhikers_guide_to_Haskell
greeter = do
  putStrLn "Hello there! May i ask your name?"
  name <- getLine
  if name == "no"
    then putStrLn "Well, sorry i asked..goodbye!"
    else putStrLn ("Well hello there " ++ name ++ ", it's nice to meet you!")

-- The above greeter "de-sugared"
greeter2 :: IO ()
greeter2 = putStrLn "Hello there! May i ask your name?"
           >> getLine
           >>= \name -> if name == "no"
                        then putStrLn "Well, sorry i asked..goodbye!"
                        else putStrLn ("Well hello there " ++ name ++ ", it's nice to meet you!")

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x

myTail :: [a] -> [a]
myTail [] = []
myTail (_:xs) = xs

-- Old version..why not make it for all monads?
-- myLiftM :: (a -> b) -> IO a -> IO b
-- myLiftM f a = a >>= \x -> return (f x)

{-  Here is a generic version of myLiftM, which has the same behavior as liftM. 
    Though the standard library chose to use do notation rather then the monadic
    bind function (>>=), they are actually the same once the do notation is
    de-sugared. Finally, notice the only thing that got changed here was the type
    signature.
-}
myLiftM :: Monad m => (a -> b) -> m a -> m b
myLiftM f a = a >>= \x -> return (f x)

--nthDigit :: Integer -> Integer -> Integer
--nthDigit n i = floor(10 * (f - floor(f)))
--  where f = n/10^(i+1)

-- Implementation of a Maybe like type
data Perhaps a = PNone
               | PJust a
               deriving (Eq,Ord,Show)

instance Functor Perhaps where
  fmap _ PNone = PNone
  fmap f (PJust x) = PJust (f x)

instance Monad Perhaps where
  (PJust a) >>= f = f a
  PNone >>= _ = PNone
  
  return a = PJust a

instance MonadPlus Perhaps where
  mzero = PNone
  
  mplus (PJust a) _ = PJust a
  mplus PNone (PJust a) = PJust a
  mplus _ _ = PNone

-- Simple Binary Tree type
data Tree a = Empty
            | Node a (Tree a) (Tree a)
            deriving (Show, Eq)

instance Ord m => Ord (Tree m) where
  _ >= Empty = True
  (Node a _ _) >= (Node b _ _) = a >= b
  _ >= _ = False
  
  _ <= Empty = True
  (Node a _ _) <= (Node b _ _) = a <= b
  _ <= _ = False

leaf :: a -> Tree a
leaf x = Node x Empty Empty

balanced :: Ord a => Tree a -> Bool
balanced Empty = True
balanced nd@(Node _ ls rs) = nd >= ls && nd <= rs && balanced ls && balanced rs

depth :: Tree a -> Int
depth Empty = 0
depth (Node _ ls rs) = 1 + max (depth ls) (depth rs)

-- A parser type
type Parser a = String -> [(a,String)]

-- Questions from Book "Programming in Haskell"
-- Excercises 5.8

-- Given an even list returns a pair of its halves
halve :: [a] -> ([a],[a])
halve xs
  | length xs `mod` 2 == 0 = (take halfLen xs, drop halfLen xs)
  | otherwise              = ([],[])
  where halfLen = (length xs `div` 2)
        
safeTailA :: [a] -> [a]
safeTailA xs = if null xs then [] else tail xs

safeTailB :: [a] -> [a]
safeTailB xs
  | null xs = []
  | otherwise = tail xs

safeTailC :: [a] -> [a]
safeTailC [] = []
safeTailC (x:xs) = xs

-- Did a version using the Maybe type for entertainment
safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (x:xs) = Just xs

myReplicate :: Int -> a -> [a]
myReplicate i e = [x | _ <- [1..i], x <- [e]]

pythagoreans :: Int -> [(Int,Int,Int)]
pythagoreans i = [(x,y,z) | x <- [1..i], y <- [1..i], z <- [1..i], x^2 + y^2 == z^2]

scalarProduct :: [Int] -> [Int] -> Int
scalarProduct xs ys = sum [x * y | (x,y) <- zip xs ys]

-- Excercise 7.8
toPowerOf :: Int -> Int -> Int
x `toPowerOf` 0 = 1
x `toPowerOf` n = x * (x `toPowerOf` (n-1))

myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs)
  | x = myAnd xs
  | otherwise = False

myAndFoldr :: [Bool] -> Bool
myAndFoldr = foldr (&&) True

myConcat :: [[a]] -> [a]
myConcat [] = []
myConcat (x:xs) = x ++ myConcat xs

myReplicateR :: Int -> a -> [a]
myReplicateR 0 _ = []
myReplicateR n e = e : myReplicateR (n-1) e

nthElem :: [a] -> Int -> a
nthElem (x:xs) 0 = x
nthElem (x:xs) n = nthElem xs (n-1)
nthElem [] _ = undefined

nthElemSafe :: [a] -> Int -> Maybe a
nthElemSafe (x:xs) 0 = Just x
nthElemSafe (x:xs) n = nthElemSafe xs (n-1)
nthElemSafe [] _ = Nothing

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem e (x:xs)
  | e == x = True
  | otherwise = myElem e xs

merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  | x < y = x:merge xs (y:ys)
  | x == y = x:y:merge xs ys
  | otherwise = y:merge (x:xs) ys

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort (take halflen xs)) (msort (drop halflen xs))
  where halflen = length xs `div` 2

-- Other random functions

increasing :: Ord a => [a] -> Bool
increasing [] = False
increasing (x:xs) = inc xs x True
  where inc [] _ bl = True
        inc (_:_) _ False = False
        inc (x:xs) a True = inc xs x (a < x)

-- Could implement the error handling for the empty list case below
-- using Maybe instead of error resulting in a type:
-- mymax :: Ord a => [a] -> Maybe a
mymax :: Ord a => [a] -> a
mymax [] = error "A empty list has no maximum"
mymax (x:xs) = aux xs x
  where aux [] y = y
        aux (x:xs) y
          | x > y = aux xs x
          | otherwise = aux xs y

-- A seemingly nicer implementation of mymax above
mymax2 :: Ord a => [a] -> Maybe a
mymax2 [] = Nothing
mymax2 (x:xs) = Just $ foldr' lrgr x xs
  where lrgr a b
          | a > b = a
          | otherwise = b

flatten :: [[a]] -> [a]
flatten [] = []
flatten (x:xs) = x ++ flatten xs

-- Note: the definition below is the same as: flatten' = foldr (++) []
flatten' :: [[a]] -> [a]
flatten' xss = flat xss []
  where flat [] acc = acc
        flat (y:ys) acc = let nacc = acc ++ y
                          in nacc `seq` flat ys nacc

-- Implementation of the square root function using fixed points *doesn't work*
sqrt' x = fix (\f y -> if ((y * y) - x) / x <= 0.0001 then y else y / x) x

-- Learning from https://en.wikibooks.org/wiki/Haskell/Existentially_quantified_types
data ShowBox = forall s. Show s => SB s

instance Show ShowBox where
  show (SB a) = show a

type HList = [ShowBox]

heterogeniusList :: HList
heterogeniusList = [SB 1, SB ['a'..'c'], SB 'd', SB 3]

-- How do i pattern match on (SB a) when a would be a list of depth n
-- Is it possible to restrict ShowBox to only hold non-list values?
-- flattenHList :: HList -> HList
-- flattenHList [] = []
-- flattenHList (x:xs) = 

-- Questions from the haskell wiki
-- url: http://www.haskell.org/haskellwiki/99_questions/1_to_10

-- 1
myLast :: [a] -> a
myLast lst = lst !! (len - 1)
	where len = length lst

myLast2 :: [a] -> a
myLast2 [] = error "No last element!"
myLast2 (x:[]) = x
myLast2 (x:xs) = myLast2 xs

-- Blank main function (can test things here)
main :: IO ()
main =  undefined
