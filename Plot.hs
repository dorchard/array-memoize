module Plot(plotDefault,writePlot,plot3d) where

import Graphics.Gnuplot.Advanced 
import Graphics.Gnuplot.File
import qualified Graphics.Gnuplot.Graph.TwoDimensional as Graph2D
import qualified Graphics.Gnuplot.Plot.TwoDimensional as Plot2D
import qualified Graphics.Gnuplot.Graph.ThreeDimensional as Graph3D
import qualified Graphics.Gnuplot.Plot.ThreeDimensional as Plot3D
import qualified Graphics.Gnuplot.Frame as Frame
import qualified Graphics.Gnuplot.Frame.OptionSet as Opts
import qualified Graphics.Gnuplot.Terminal.PNG as PNG
import qualified Graphics.Gnuplot.Terminal.PostScript as PS

simple2d :: Plot2D.T Double Double
simple2d =
   Plot2D.function Graph2D.lines
      (Plot2D.linearScale 100 (-10,10)) sin

plot3d :: Double -> Double -> (Double,Double) -> (Double, Double) -> 
          String -> String -> String -> 
          (Double -> Double -> Double) -> Frame.T (Graph3D.T Double Double Double)
plot3d dx dy (sx, nx) (sy, ny) xlabel ylabel zlabel fun =
  let meshNodesX = [sx,dx..nx] -- Plot3D.linearScale ((floor ((nx / dx)::Double))) (0, nx)
      meshNodesY = [sy,dy..ny] -- Plot3D.linearScale ((floor ((nt / dt)::Double))) (0, nt)
      frameOpts =
          Opts.xRange3d (0, nx) $
          Opts.yRange3d (0, ny) $
          Opts.xLabel xlabel $
          Opts.yLabel ylabel $
          Opts.zLabel zlabel $
          Opts.grid True $
          Opts.deflt
  in
   Frame.cons frameOpts (Plot3D.surface meshNodesX meshNodesY fun)

writePlot p fname = plot (PS.color (PS.eps (PS.cons fname))) p
                    --plot (PNG.cons fname) p

