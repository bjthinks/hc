module PrimeList (primeList, test_PrimeList) where

import Test.HUnit

primeList :: [Integer]
primeList = primeListFrom 2
  where
    primeListFrom :: Integer -> [Integer]
    primeListFrom n
      | isPrime n  =  n : primeListFrom (n+1)
      | otherwise  =      primeListFrom (n+1)

    isPrime :: Integer -> Bool
    isPrime 2  =  True
    isPrime n  =  not $ divisibleByList n primeList

    divisibleByList :: Integer -> [Integer] -> Bool
    divisibleByList n (p:ps)
      | n `mod` p == 0  =  True
      | p * p > n       =  False
      | otherwise       =  divisibleByList n ps

test_PrimeList = [
  primeList !! 1000 ~?= 7927,
  take (length knownPrimes) primeList ~?= knownPrimes
  ]
  where
    knownPrimes = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47,
                   53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107,
                   109, 113, 127, 131, 137, 139, 149, 151, 157, 163, 167,
                   173, 179, 181, 191, 193, 197, 199]

{-
trialDivide :: Integer -> [Integer]
trialDivide n = try n primeList
  where
    try :: Integer -> [Integer] -> [Integer]
    try n (p:ps)
      | p*p > n = if n > 1 then [n] else []
      | n `mod` p == 0 = p : (try (n `div` p) (p:ps))
      | otherwise = try n ps

isEven :: Integer -> Bool
isEven x
  | x `mod` 2 == 0 = True
  | otherwise = False

-- Raise an integer to the power of another integer mod a third integer
powMod :: Integer -> Integer -> Integer -> Integer
powMod a x m
  | x == 0 = 1
  | isEven x = powMod ((a*a) `mod` m) (x `div` 2) m
  | otherwise = (a * (powMod a (x-1) m)) `mod` m

-- squareMod a m = powMod a 2 m
squareMod :: Integer -> Integer -> Integer
squareMod a m = (a*a) `mod` m

values :: Integer -> Integer -> [Integer]
values p a = v
  where
    -- (p-1) = d * 2^s
    s = evenLog (p-1)
    d = (p-1) `div` (2 ^ s)
    evenLog x
      | x == 0            =  0
      | (x `mod` 2) == 1  =  0
      | otherwise         =  1 + evenLog (x `div` 2)
    v = take (fromInteger (s+1)) (iterate (flip squareMod p) (powMod a d p))

-- Check if an integer is a strong probable prime to a base
-- When does this offer a speedup vs trial division?  For a prime
-- number p with log p bits, this test repeated say 40 times would
-- require 40 * (3 log p) arithmetic operations on numbers of
-- magnitude p, whereas trial division would require
-- (sqrt p) / (log sqrt p) such operations.
-- 120 log p = sqrt p / log sqrt p
-- 60 (log p)^2 = sqrt p
-- 60 L^2 = 2^(L/2)
-- L = 16
-- p = 2^16
strongProbablePrime :: Integer -> Integer -> Bool
strongProbablePrime p a
  | p < 0   =  strongProbablePrime (-p) a -- p prime iff -p prime
  | p < 2   =  False -- 0 and 1 are not prime
  | a >= p  =  strongProbablePrime p (a `mod` p)
  | a == 0  =  True -- or Nothing or error, nonsense input
  | otherwise  =  runTest
    where
      runTest = if head v == 1 then True else valuesGood v
      v = values p a
      valuesGood (y:1:xs) = y == p-1
      valuesGood (x:xs) = valuesGood xs
      valuesGood _ = False
-}
