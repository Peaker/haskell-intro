{- Ordinary comment -}

{-# OPTIONS -Wall #-}

import Control.Arrow ((&&&))
import Data.List (group)

{- And with the #'s it's a pragma ^ -}

-- We can pass compiler options via "pragma"


-- (:) :: a -> [a] -> [a]

-- mod :: Integer -> Integer -> Integer -- <- Lie to children

primes :: [Integer]
primes = sieve [2..]
  where
    sieve (p:xs) = p : sieve [x|x <- xs, x `mod` p > 0]

-- Note we may want to explicitly add the case:
-- sieve [] = error "sieve is applied with a non-empty list!"
-- To satisfy the compiler (which yells about non-exhaustive pattern matches)

-- (+) :: Integer -> Integer -> Integer -- <- Lie to children
-- tail :: [a] -> [a]
-- zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- sum :: [Integer] -> Integer -- <- Lie to children
-- iterate :: a -> (a -> a) -> [a]
-- reverse :: [a] -> [a]

type Digit = Integer

decodeDigits :: Digit -> [Digit] -> Integer
decodeDigits base digits =
  sum (zipWith (*) (reverse digits) powers)
  where
    powers = iterate (* base) 1

-- No lie to children here!
sort :: Ord a => [a] -> [a]
sort [] = []
sort (p:xs) =
  sort [ x | x <- xs, x <= p ] ++
  [ p ] ++
  sort [ x | x <- xs, x > p ]

-- Alternatively:

-- sort (p:xs) =
--   sort (filter (<= p) xs) ++
--   [ p ] ++
--   sort (filter (> p) xs)

-- Histogram:

-- group :: Eq a => [a] -> [[a]]

-- (&&&) :: (a -> o1) -> (a -> o2) -> a -> (o1, o2)  -- Lie to children
-- (.) :: (b -> c) -> (a -> b) -> a -> c   -- Note that (a -> c) can be in parens too, if it makes it clearer

-- head is an EVIL FUNCTION! Use only when you can prove your list is
-- non-empty or crash at runtime like a Pythoneer (except with a much
-- less useful stack trace)!

-- head :: [a] -> a

histogram :: Ord a => [a] -> [(a, Int)]
histogram = map (head &&& length) . group . sort
-- Reminds anyone of unix pipes?

-- > histogram "Hello world"
-- [(' ',1),('H',1),('d',1),('e',1),('l',3),('o',2),('r',1),('w',1)]

-- snd :: (a, b) -> b
prop_histogram_length :: [Int] -> Bool
prop_histogram_length xs = sum (map snd (histogram xs)) == length xs

-- Here we can use "head" because "group" is guaranteed to return
-- non-empty lists (but unfortunately it does not specify this in its
-- types).
