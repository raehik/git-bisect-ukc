-- Cool. https://wiki.haskell.org/The_Fibonacci_sequence#Using_2x2_matrices

import Data.List (transpose)

newtype Matrix a = Matrix [[a]] deriving (Eq, Show)

instance Num a => Num (Matrix a) where
    Matrix as + Matrix bs = Matrix (zipWith (zipWith (+)) as bs)
    Matrix as - Matrix bs = Matrix (zipWith (zipWith (-)) as bs)
    Matrix as * Matrix bs =
        Matrix [[sum $ zipWith (*) a b | b <- transpose bs] | a <- as]
    negate (Matrix as) = Matrix (map (map negate) as)
    fromInteger x = Matrix (iterate (0:) (fromInteger x : repeat 0))
    abs m = m
    signum _ = 1

apply :: Num a => Matrix a -> [a] -> [a]
apply (Matrix as) b = [sum (zipWith (*) a b) | a <- as]

fib n = head (apply (Matrix [[0,1], [1,1]] ^ n) [0,1])
