import Data.Function.ArrayMemoize
import Criterion.Main 

-- Compiled with: ghc -O2 --make heat-benchmark.hs
-- Run with:      ./benchmark -o heat-benchmark-results.html

foo x = undefined -- discretize delx (continuize delx x)

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

-- Discrete heat function (requires writing unfixed version (or used macro), semantically easy)
heatD :: (Int, Int) -> Float
heatD = discreteMemoFix ((0, 0), (nx, nt)) (delx, delt) heat'

-- Quantized heat function (as above, but semantically can be tricky around edges)
heatQ :: (Float, Float) -> Float
heatQ = quantizedMemoFix ((0, 0), (nx, nt)) (delx, delt) heat'

heat'' :: (Float, Float) -> Float
heat'' (0.0, t) = 1
heat'' (3,   t) = 0
heat'' (x, 0.0) = 0
heat'' (x, t) = (h' x) + r * (h' (x - delx) - 2 * (h' x) + h' (x + delx))
                   where h' x = heatQB (x, t - delt)
                         r    = alpha * (delt / (delx * delx))

-- Discrete heat function (via quantization) (syntactically the easiest, but slower than D)
heatQB = quantizedMemo ((0, 0), (nx, nt)) (delx, delt) heat''
heatD2 :: (Int, Int) -> Float
heatD2 = heatQB . (continuize (delx,delt))

-- Alternate discrete definition using fixed-point outside of the library (a bit cumbersome- manual discretization used)
heatA :: (Float, Float) -> Float
heatA (0.0, t) = 1
heatA (3,   t) = 0
heatA (x, 0.0) = 0
heatA (x, t) = (h' x) + r * (h' (x - delx) - 2 * (h' x) + h' (x + delx))
                   where h' x = heatD3 (discretize delx x, (discretize delt t) - 1)
                         r       = alpha * (delt / (delx * delx))

heatD3 :: (Int, Int) -> Float
heatD3 = discreteMemo ((0, 0), (nx, nt)) (delx, delt) heatA

-- The following is a nice style for writing it all together- and reasonably fast - faster than D2
heat :: (Int, Int) -> Float
heat = let   -- Parameter setup
             alpha = 0.23
             dt = 0.05 
             dx = 0.1 
             nt = 5
             nx = 3

             r = alpha * (dt / (dx * dx))

             -- Continuous heat equation
             h :: (Float, Float) -> Float
             h (x, t) | x == 0.0  = 1
                      | x == nx   = 0
                      | t == 0.0  = 0
                      | otherwise = h'(x, t-dt) + r * (h'(x-dx, t-dt) - 2*(h'(x, t-dt)) + h'(x+dx, t-dt))

             -- Faster quantized heat equation
             h' = quantizedMemo  ((0, 0), (nx, nt))  (dx, dt) h

        -- Finally return discrete version 
        in discrete  h'  (dx, dt)

heatDub :: (Int, Int) -> Double
heatDub = let   -- Parameter setup
             alpha = 0.23
             dt = 0.05 
             dx = 0.1 
             nt = 5
             nx = 3

             r = alpha * (dt / (dx * dx))

             -- Continuous heat equation
             h :: (Double, Double) -> Double
             h (x, t) | x == 0.0  = 1
                      | x == nx   = 0
                      | t == 0.0  = 0
                      | otherwise = h'(x, t-dt) + r * (h'(x-dx, t-dt) - 2*(h'(x, t-dt)) + h'(x+dx, t-dt))

             -- Faster quantized heat equation
             h' = quantizedMemo   ((0, 0), (nx, nt))  (dx, dt) h

          -- Finally return discrete version 
          in discrete  h'  (dx, dt)

fix f = f (fix f)

main = defaultMain [
        bcompare[ bench "heat fix (0.2,0.1)" $ whnf (fix heat') (0.2,0.1),
                -- These reults turn out as the same as the previous as the 
                -- memoization memoizes the whole thing

                --  bench "heatD '(0.2, 0.1)'" $ whnf heatD (2,2),                       
                --  bench "heatQ (0.2,0.1)"    $ whnf heatQ (0.2,0.1),
                --  bench "heatQB (0.2,0.1)"    $ whnf heatQB (0.2,0.1),
                --  bench "heatD2 '(0.2,0.1)'" $ whnf heatD2  (2,2),
                --  bench "heatD3 '(0.2,0.1)'" $ whnf heatD3 (2,2),

                  bench "heat '(1.5, 2.5)'" $ whnf heat (15,50),
                  bench "heat double '(1.5, 2.5)'" $ whnf heatDub (15,50),
                  bench "heatD '(1.5, 2.5)'" $ whnf heatD (15,50),
                  bench "heatQ (1.5,2.5)"    $ whnf heatQ (1.5,2.5),
                  bench "heatQB (1.5,2.5)"   $ whnf heatQB (1.5,2.5),
                  bench "heatD2 '(1.5,2.5)'" $ whnf heatD2  (15,50),
                  bench "heatD3 '(1.5,2.5)'" $ whnf heatD3 (15,50)]
       ]
