module Recursion where

fib :: Integer -> Integer
fib n = go n 0 1
	where
		go 0 m k = 0
		go 1 m k = m+k
		go n m k = go (n-1) (m+k) m


binom :: Double -> Double -> Int -> Double
binom a b n = go a b n 0 1.0 0.0
	where
		go :: Double -> Double -> Int -> Int -> Double -> Double -> Double
		go a b n k comb s = 
			if n == k
				then (s+(comb*b^n))
				else go a b n (k+1) (comb*(fromIntegral (n-k))/(fromIntegral (k+1))) (s+(comb*a^(n-k)*b^k))
