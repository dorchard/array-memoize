Memoization combinators are great for providing high-performance Haskell programs,
but they can be even faster if memoization is performed on a finite, discrete domain
as an array can then be used.

This package provides various combinators for doing just this, including also 
combinators for quanitzing and discretizing Float/Double-valued functions.

Example:


    fibA :: Int -> Int
    fibA 0 = 1
    fibA 1 = 1
    fibA n = fibB (n - 1) + fibB (n - 2)
   
    fibB = arrayMemo (0, 1000) fibA 
