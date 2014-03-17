import Data.Function.ArrayMemoize

-- Example:

-- Fibonacci (pre-fixed point)
fib' :: (Int -> Int) -> Int -> Int
fib' _ 0 = 1
fib' _ 1 = 1
fib' rec n = rec (n - 1) + rec (n - 2)

fib = arrayMemo fib' (0, 100)
