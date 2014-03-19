import Data.Function.ArrayMemoize

alpha = 0.23

delt = 0.05 :: Float
delx = 0.1  :: Float
nt = 5      :: Float
nx = 3      :: Float

heat' :: ((Float, Float) -> Float) -> (Float, Float) -> Float
heat' _ (0.0, t) = 1
heat' _ (3,   t) = 0
heat' _ (x, 0.0) = 0
heat' h (x, t) = (h' x) + r * (h' (x - delx) - 2 * (h' x) + h' (x + delx))
                   where h' x = h (x, t - delt)
                         r       = alpha * (delt / (delx * delx))

-- Discrete heat function
heat :: (Int, Int) -> Float
heat = discreteMemoFix heat' ((0, 0), (nx, nt)) (delx, delt)

-- Quantized heat function
heatQ :: (Float, Float) -> Float
heatQ = quantizedMemoFix heat' ((0, 0), (nx, nt)) (delx, delt)

-- Alternate discretized heat function using fixed-point outside of the library 
heatQA :: (Float, Float) -> Float
heatQA (0.0, t) = 1
heatQA (3,   t) = 0
heatQA (x, 0.0) = 0
heatQA (x, t) = (h' x) + r * (h' (x - delx) - 2 * (h' x) + h' (x + delx))
                   where h' x = heatQB (x, t - delt)
                         r       = alpha * (delt / (delx * delx))

heatQB = quantizedMemo heatQA ((0, 0), (nx, nt)) (delx, delt)
heatD :: (Int, Int) -> Float
heatD = discrete heatQB (delx,delt)

