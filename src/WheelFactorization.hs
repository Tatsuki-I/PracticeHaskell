module WheelFactorization where

m3Primes :: [Int]
m3Primes =  [2, 3, 5]

m3Cycle :: [Int]
m3Cycle =  [6, 4, 2, 4, 2, 4, 6, 2]

genCycle   :: Int -> [Int]
genCycle n =  f ls
              where primes = take n $ eratosthenes1 [2 .. 1000]
                    ls =  1 : (drop n $ eratosthenes1 $ take (product primes) [2 ..])
                    f (_ : []) = []
                    f (x1 : x2 : xs) = (x2 - x1) : f (x2 : xs)
{-
   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30
  31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60
-}

--genCycle :: [Integer] -> [Bool]
genCycleLs xs =  foldl1 (++) (map (\x -> replicate (x - 1) False ++ [True]) xs)


wheelFactorization      :: Int -> Int -> [Int]
wheelFactorization ln n =  (++) primes
                                $ map fst . filter snd
                                  $ zip (drop (last primes) [1 ..])
                                        (drop (last primes)
                                              (True : (take (ln - 1)
                                                       . foldr1 (++)
                                                       . repeat
                                                       . genCycleLs . genCycle) n))
                           where primes = take n $ eratosthenes1 [2 .. 10000]

eratosthenes          :: Integral a => [a] -> [a]
eratosthenes []       =  []
eratosthenes (x : xs) =  x : eratosthenes (filter (\y -> (y `mod` x) /= 0) xs)

eratosthenes1          :: Integral a => [a] -> [a]
eratosthenes1 xs = eratosthenes1' xs []
eratosthenes1' [] ys = ys
eratosthenes1' (x : xs)  ys =  eratosthenes1' (filter (\y -> (y `mod` x) /= 0) xs) (ys ++ [x])

compLs :: Int -> Int -> Bool
compLs ln n =  (and . (zipWith (==) (eratosthenes1 [2 .. ln])) . eratosthenes1 . wheelFactorization ln) n
