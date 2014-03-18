import Data.Function.ArrayMemoize
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

fib2 :: Int -> Int
fib2 = uarrayMemoFix fib' (0,1000)


-- IO variant

fibIO' :: (Int -> IO Int) -> Int -> IO Int
fibIO' _ 0 = return 1
fibIO' _ 1 = return 1
fibIO' rec n = do a <- rec (n - 1)
                  b <- rec (n - 2)
                  return (a + b)

fibIO :: Int -> IO Int
fibIO = uarrayMemoFixIO fibIO' (0,1000)

main = defaultMain [
        bgroup "fib" [ bench "fib" $ whnf fib 1000, 
                       bench "fib2" $ whnf fib2 1000, 
                       bench "fibIO" $ whnf fibIO 1000 ] 
       ]

{- defaultMain [
        bench "fib 1000" $ (whnf fib 1000),
        bench "fib2 1000" $ (whnf fib2 1000)
       ]
-}