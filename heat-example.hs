import Data.Function.ArrayMemoize

heat :: (Int, Int) -> Double
heat = let   -- Parameter setup
             dt = 0.05 
             dx = 0.1 
             nt = 5
             nx = 3

             alpha = 0.23
             r = alpha * (dt / (dx * dx))

             -- Continuous heat equation
             h :: (Double, Double) -> Double
             h (x, t) | x == 0.0  = 1
                      | x == nx   = 0
                      | t == 0.0  = 0
                      | otherwise = h'(x, t-dt) + r * (h'(x-dx, t-dt) - 2*(h'(x, t-dt)) + h'(x+dx, t-dt))

             -- Faster finite, quantized heat equation
             h' = quantizedMemo ((0, 0), (nx, nt)) (dx, dt) h

        -- Return discretized version 
        in discrete h' (dx, dt)