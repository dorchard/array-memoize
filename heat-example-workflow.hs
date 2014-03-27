{-# LANGUAGE TemplateHaskell, NoMonomorphismRestriction, FlexibleInstances #-}

import Data.Function.ArrayMemoize
import Language.Haskell.Unfix -- http://github.com/dorchard/unfix

import LaTeX
import Plot

-- Parameter setup
class Params t where
    dt :: t
    dx :: t
    nt :: t
    nx :: t
    alph :: t
    parens :: t -> t

instance Params Double where
    dt = 0.1
    dx = 0.05
    nt = 25
    nx = 10
    alph = 0.01
    parens = id

r = alph * (dt / (dx * dx))

-- Continuous heat equation
unfix [d| heat (x, t) 
            | x == 0  = 1
            | x == nx = 1 -- heat(x-dx, t)
            | t == 0  = 0
            | otherwise = heat(x, t-dt) + r * parens (heat(x-dx, t-dt) - 
                                                   2*(heat(x, t-dt)) + heat(x+dx, t-dt)) |]

heatQ = quantizedMemoFix ((0, 0), (nx, nt)) (dx, dt) heat



instance Params LaTeX where
    dx = deltau <> (fromString "x")
    dt = deltau <> (fromString "t")
    nt = mathit $ fromString "nt"
    nx = mathit $ fromString "nx"
    alph = alpha
    parens x = fromString "(" <> x <> fromString ")"


main = do let plot = plot3d dx dt (0.0, nx) (0.0, nt) "x distance" "t time" "e heat" (curry heatQ)
              heatTex = fixLatexCases "h" heat
                          [(0, fromString "t"), 
                           (nx, fromString "t"),
                           (fromString "x", 0),
                           (fromString "x", fromString "t")]
          plotX11 plot
          putStrLn "write out too?"
          guard <- readLn
          if (guard == "y") then
            do writePlot plot "heat.eps"
               writeLatexFigure heatTex "heat.eps" "heat.tex"
          else
            return ()
          
