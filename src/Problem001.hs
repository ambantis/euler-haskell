{-# Project Euler 001 - Multiples of 3 and 5 #-}
------------------------------------------------
--
-- Module      :  Problem001
-- User        :  Alexandros Bantis
-- Date        :  2015-01-29
--
-- If we list all the natural numbers below 10 that are multiples of 3 or 5,
-- we get 3,5,6,9. The sum of these multiples is 23. Find the sum of all
-- multiples of 3 or 5 below 1000
------------------------------------------------

module Problem001
( sumMultiples
) where

sumMultiples :: Int -> [Int] -> Int
sumMultiples n multiples = sum . filter (\ x -> isMultipleOf x multiples) $ naturalsTo n

isMultipleOf :: Int -> [Int] -> Bool
isMultipleOf n = foldl (\ acc x -> (mod n x == 0) || acc) False

naturalsTo :: Int -> [Int]
naturalsTo n = takeWhile (\ a -> a < n) [1..]

















