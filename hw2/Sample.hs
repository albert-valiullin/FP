module Sample where

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

acker :: Int -> Int -> Int
acker 0 n = n + 1
acker m 0 =
	if m > 0
		then acker (m - 1) 1
		else error "m < 0"
acker m n =
	if m > 0 && n > 0
		then acker (m - 1) (acker m (n - 1)) 
		else error  "m < 0 or n < 0"
