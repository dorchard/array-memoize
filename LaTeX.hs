{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module LaTeX((<>), fromString, LaTeX(..),alpha,deltau,fixLatex,fixLatexCases,doLatex,writeLatexFigure,mathit) where

--import Prelude hiding (Bool)

import Text.LaTeX.Base hiding (alph)
import Text.LaTeX.Packages.AMSMath 
import Text.LaTeX.Packages.Graphicx
import Text.LaTeX.Base.Render

import Data.Text hiding (map)

-- type Bool = Latex

{- Some stuff missing from LaTeX package -}

instance Ord LaTeX where

{- instance Ord LaTeX where
    x <= y = (read . unpack . render $ x) <= (read . unpack . render $ y) -}

-- B (x <> (fromString "\\leq") <> y)

instance Real LaTeX where
instance RealFrac LaTeX where

{- 
data Lift s a = L a LaTeX

class Unlift s a where
      unlift :: Lift s a -> s
      lifte :: s -> Lift s a
instance Unlift LaTeX a where
      unlift (L _ x) = x
      lifte x = L undefined x
instance Unlift Float Float where
      unlift (L x _) = x
      lifte x = L x undefined

(<|>) :: a -> LaTeX -> Lift s a
x <|> t = L x t

instance (Ord a, Ord s, Unlift s a) => Ord (Lift s a) where
  x <= y = (unlift x) <= (unlift y) 
instance (Show a, Num a, Num s, Unlift s a) => Num (Lift s a) where
    a + b = lifte $ (unlift a) + (unlift b)
    a * b = lifte $ (unlift a) * (unlift b)
    a - b = lifte $ (unlift a) - (unlift b)
    fromInteger x = L (fromInteger x) (fromString $ show x)
instance (Show a, Fractional a, Fractional s, Unlift s a) => Fractional (Lift s a) where
    a / b = lifte $ (unlift a) / (unlift b)
instance (Eq s, Eq a, Unlift s a) => Eq (Lift s a) where
    a == b = (unlift a) == (unlift b)

-}

{- LaTeX fixed point #-}
    
fixLatex :: String -> (((LaTeX, LaTeX) -> LaTeX) -> (LaTeX, LaTeX) -> LaTeX) -> (LaTeX, LaTeX) -> LaTeX
fixLatex recName f = \(x, y) -> ((fromString recName) ^: x !: y) `equiv`  (f (\(x, y) -> (fromString recName) ^: x !: y) (x, y))

fixLatexD :: String -> (LaTeX -> LaTeX -> LaTeX) -> (((LaTeX, LaTeX) -> LaTeX) -> (LaTeX, LaTeX) -> LaTeX) -> (LaTeX, LaTeX) -> LaTeX
fixLatexD recName delim f = \(x, y) -> delim ((fromString recName) ^: x !: y) (fromString "=" <> (f (\(x, y) -> (fromString recName) ^: x !: y) (x, y)))

fixLatexCases recName f cases = align $ map (fixLatexD recName (&) f) cases

doLatex eqn name = renderFile (name ++ ".tex") (wrapped eqn)
                    where wrapped eqn = (documentclass [] article) <> usepackage [] "amsmath" <> (document eqn) -- (equiv (mathit $ fromString name) eqn)))

writeLatexFigure eqn figname name = 
       renderFile name ((documentclass [] article) 
                                     <> usepackage [] "amsmath" 
                                     <> usepackage [] "graphicx"
                                    <> (document (eqn <> (includegraphics [] figname))))