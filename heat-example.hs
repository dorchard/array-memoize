import Data.Function.ArrayMemoize


heat :: (Int, Int) -> Double
heat = let   -- Parameter setup
             alpha = 0.23
             dt = 0.05 
             dx = 0.1 
             nt = 5
             nx = 3

             r = alpha * (dt / (dx * dx))

             -- Continuous heat equation
             h :: (Double, Double) -> Double
             h (0.0, t) = 1
             h (3.0, t) = 0
             h (x, 0.0) = 0
             h (x, t)   = h' (x, t-dt) + r * (h' (x-dx, t-dt) - 2*(h' (x, t-dt)) + h' (x+dx, t-dt))

             -- Faster quantized heat equation
             h' = quantizedMemo h ((0, 0), (nx, nt)) (dx, dt)

        -- Finally return discrete version 
        in discrete h' (dx, dt)