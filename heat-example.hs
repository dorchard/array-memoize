import Data.Function.ArrayMemoize

alpha = 0.23

delt = 0.05 :: Float
delx = 0.1  :: Float
nx = 1.0    :: Float
nt = 2.1    :: Float

heat' :: ((Float, Float) -> Float) -> (Float, Float) -> Float
heat' _ (0.0, t) = 1
heat' _ (x, 0.0) = 0
heat' h (x, t) = (h_old x) + r * (h_old (x - delt) - 2 * (h_old x) + h_old (x + delx))
                   where h_old x = h (x, t - delt)
                         r       = alpha * (delt / (delx * delx))

heat :: (Float, Float) -> Float
heat = discMemo heat' ((0, 0), (nx, nt)) (delx, delt)

foo = enumFromThenTo (0, 0) (delx, delt) (nx, nt)

heatA = discMemoA heat' ((0, 0), (nx, nt)) (delx, delt)