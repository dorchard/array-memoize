{-# LANGUAGE TemplateHaskell #-}

import Data.Function.ArrayMemoize
import Language.Haskell.Unfix -- http://github.com/dorchard/unfix

-- Parameter setup
dt = 0.05 
dx = 0.1 
nt = 5
nx = 3

alpha = 0.23
r = alpha * (dt / (dx * dx))

-- Continuous heat equation
refix [| discreteMemoFix ((0, 0), (nx, nt)) (dx, dt) |]
  
      [d| heat (x, t) 
           | x == 0.0  = 1
           | x == nx   = 0
           | t == 0.0  = 0
           | otherwise = heat(x, t-dt) + r * (heat(x-dx, t-dt) - 2*(heat(x, t-dt)) + heat(x+dx, t-dt)) |]

