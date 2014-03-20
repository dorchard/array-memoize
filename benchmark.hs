import Data.Function.ArrayMemoize
import Data.Function.Memoize
import Criterion.Main 

-- Compiled with: ghc -O2 --make benchmark.hs
-- Run with:      ./benchmark -o benchmark-results.html


-- Example:

fib' :: (Int -> Int) -> Int -> Int
fib' _ 0 = 1
fib' _ 1 = 1
fib' rec n = rec (n - 1) + rec (n - 2)

fibIO' :: (Int -> IO Int) -> Int -> IO Int
fibIO' _ 0 = return 1
fibIO' _ 1 = return 1
fibIO' rec n = do a <- rec (n - 1)
                  b <- rec (n - 2)
                  return (a + b)

-- Benchmarking - comparision with the Data.Function.Memoize library

fibS s = arrayMemoFix (0, s) fib'
fibIOS s = uarrayMemoFixIO (0,s) fibIO'

main = defaultMain [
        bcompare[ bench "memoFix 1000" $ whnf (memoFix fib') 1000,
                  bench "memoFix 5000" $ whnf (memoFix fib') 5000, 
                  bench "arryMemoFix 1000/1000" $ whnf (fibS 1000)  1000, 
                  bench "arrayMemoFix 1000/5000" $ whnf (fibS 5000) 1000, 
                  bench "arrayMemoFix 5000/5000" $ whnf (fibS 5000) 5000, 
                  bench "uarrayMemoFixIO 1000/1000" $ whnf (fibIOS 1000) 1000,
                  bench "uarrayMemoFixIO 1000/5000" $ whnf (fibIOS 5000) 1000,
                  bench "uarrayMemoFixIO 5000/5000" $ whnf (fibIOS 5000) 5000] 
       ]

