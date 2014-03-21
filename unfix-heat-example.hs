import Data.Function.ArrayMemoize
import Unfix

alpha = 0.23

delt = 0.05 :: Float
delx = 0.1  :: Float
nt = 5      :: Float
nx = 3      :: Float

unfix [d| heat' :: (Float, Float) -> Float
          heat' (0.0, t) = 1
          heat' (3,   t) = 0
          heat' (x, 0.0) = 0
          heat' (x, t)   = (h' x) + r * (h' (x - delx) - 2 * (h' x) + h' (x + delx))
                              where h' x = heat' (x, t - delt)
                                    r    = alpha * (delt / (delx * delx)) |]

-- Discrete heat function
heat :: (Int, Int) -> Float
heat = discMemoFix heat' ((0, 0), (nx, nt)) (delx, delt)

-- Quantized heat function
heatQ :: (Float, Float) -> Float
heatQ = quantMemoFix heat' ((0, 0), (nx, nt)) (delx, delt)
