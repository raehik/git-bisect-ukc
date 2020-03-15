import Control.Parallel

--fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib 2 = 1
fib n = fib (n-2) + fib (n-1)

--pfib :: Int -> Int
pfib 0 = 0
pfib 1 = 1
pfib 2 = 1
pfib n = par n1 (seq n2 (n1 + n2))
    where
        n1 = pfib (n-1)
        n2 = pfib (n-2)
