import Data.Function.ArrayMemoize


heat :: (Int, Int) -> Double
heat = let   -- Parameter setup
             alpha = 0.23
             delt = 0.05 
             delx = 0.1 
             nt = 5
             nx = 3

             r = alpha * (delt / (delx * delx))

             -- Continuous heat equation
             h :: (Double, Double) -> Double
             h (0.0, t) = 1
             h (3.0, t) = 0
             h (x, 0.0) = 0
             h (x, t)   = h' (x, t') + r * (h' (x-delx, t') - 2*(h' (x, t')) + h' (x+delx, t'))
                            where t'= t - delt

             -- "Quantized" and fast heat equation
             h' = quantizedMemo h ((0, 0), (nx, nt)) (delx, delt)

        -- Return discrete version of the heat equation
        in discrete h' (delx, delt)