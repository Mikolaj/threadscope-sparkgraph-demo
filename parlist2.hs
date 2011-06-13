{-# LANGUAGE BangPatterns #-}

-- see parlist.hs

import Data.List
import Control.Parallel
import Control.Parallel.Strategies

fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

main =
  let xs = [ fib (n `mod` 10) | n <- [0..300000] ] `using` parListWHNF
   in print (sum xs)

parListWHNF :: Strategy [a]
parListWHNF xs = go xs `pseq` return xs
  where -- go :: [a] -> [a]
    go []     = []
    go (y:ys) = y `par` go ys

parBufferWHNF :: Int -> Strategy [a]
parBufferWHNF n0 xs0 = return (ret xs0 (start n0 xs0))
  where -- ret :: [a] -> [a] -> [a]
        ret (x:xs) (y:ys) = y `par` (x : ({-x `seq`-} ret xs ys))
        ret xs     _      = xs

        -- start :: Int -> [a] -> [a]
        start 0   ys     = ys
        start !_n []     = []
        start !n  (y:ys) = y `par` start (n-1) ys
