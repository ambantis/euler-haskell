{-# Project Euler 002 - Even Fibonacci numbers #-}
------------------------------------------------
--
-- Module      :  Problem002
-- User        :  Alexandros Bantis
-- Date        :  2015-01-30
--
-- Each new term in the Fibonacci sequence is generated by adding the previous
-- two terms. By starting with 1 and 2, the first 10 terms will be
--           1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...
-- By considering the term in the Fibonacci sequence whose values to not exceed
-- four million, find the sum of the even-valued terms.
------------------------------------------------

module Problem002
( sumSomeFibosTo
) where

-- sumSomeFibosTo (\ x -> rem x 2 == 0) 4000000

sumSomeFibosTo :: (Integer -> Bool) -> Integer -> Integer
sumSomeFibosTo f n = sum . filter f $ fibosTo n


fibosTo :: Integer -> [Integer]
fibosTo ceiling
  | ceiling < 1 = []
  | ceiling < 2 = [1]
  | otherwise = growFibos [2,1]
    where growFibos :: [Integer] -> [Integer]
          growFibos fibos@(x1:x2:_)
            | x1 > ceiling = tail fibos
            | otherwise = growFibos $ (x1+x2) : fibos

