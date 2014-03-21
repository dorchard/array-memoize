{-# LANGUAGE TemplateHaskell #-}

import Data.Function.ArrayMemoize
import Language.Haskell.Unfix

-- Example:

-- Fibonacci (pre-fixed point)

unfix [d| fib' :: Int -> Int
          fib' 0 = 1
          fib' 1 = 1
          fib' n = fib' (n - 1) + fib' (n - 2) |]

-- Memoizes fib between 0 and 100 (after that it is a run-time error)

fib :: Int -> Int
fib = arrayMemoFix fib' (0, 1000)

