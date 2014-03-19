import Data.Function.ArrayMemoize

alpha = 0.23

delt = 0.05 :: Float
delx = 0.1  :: Float
nt = 5      :: Float
nx = 3      :: Float

heat' :: ((Float, Float) -> Float) -> (Float, Float) -> Float
heat' _ (0.0, t) = 1
heat' _ (3  , t) = 0
heat' _ (x, 0.0) = 0
heat' h (x, t) = (h' x) + r * (h' (x - delx) - 2 * (h' x) + h' (x + delx))
                   where h' x = h (x, t - delt)
                         r       = alpha * (delt / (delx * delx))

heat :: (Float, Float) -> Float
heat = discMemoFix heat' ((0, 0), (nx, nt)) (delx, delt)

