import Data.Function.ArrayMemoize
import Data.Function.Memoize
import Criterion.Main 

-- Example:

-- Fibonacci (pre-fixed point)
fib' :: (Int -> Int) -> Int -> Int
fib' _ 0 = 1
fib' _ 1 = 1
fib' rec n = rec (n - 1) + rec (n - 2)

-- Memoizes fib between 0 and 100 (after that it is a run-time error)

fib :: Int -> Int
fib = arrayMemoFix fib' (0, 1000)

-- IO variant

fibIO' :: (Int -> IO Int) -> Int -> IO Int
fibIO' _ 0 = return 1
fibIO' _ 1 = return 1
fibIO' rec n = do a <- rec (n - 1)
                  b <- rec (n - 2)
                  return (a + b)

fibIO :: Int -> IO Int
fibIO = uarrayMemoFixIO fibIO' (0,1000)

-- Benchmarking - comparision with the Data.Function.Memoize library

fibS s = arrayMemoFix fib' (0, s)
fibIOS s = uarrayMemoFixIO fibIO' (0,s)

main = defaultMain [
        bcompare[ bench "memoFix" $ whnf (memoFix fib') 1000, 
                  bench "arryMemoFix" $ whnf fib 1000, 
                  bench "arrayMemoFix 50000" $ whnf (fibS 5000) 1000, 
                  bench "uarrayMemoFixIO" $ whnf fibIO 1000,
                  bench "uarrayMemoFixIO 50000" $ whnf (fibIOS 5000) 1000 ] 
       ]

