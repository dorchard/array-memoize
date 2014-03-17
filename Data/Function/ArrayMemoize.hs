{-# LANGUAGE FlexibleContexts #-}

module Data.Function.ArrayMemoize where

import Data.Array.MArray
import Data.Array.Unboxed
import Data.Array.ST
import Control.Monad.ST

-- Memoize a function as an array over a finite domain

arrayMemo :: (Ix a, Discrete b) => ((a -> b) -> (a -> b)) -> (a, a) -> a -> b
arrayMemo f (l, u) =
     let cache = runSTArray (do cache <- (newUArray_ (l, u)) 
                                mapM_ (\x -> writeUArray cache x (f' x)) (range (l, u))
                                return cache)
         f' = f (\x -> cache ! x)
     in f' 

{-

 The following defines the subset of types for which we can do array 
 memoization (the unboxed class). 

-}
 
class IArray UArray a => Discrete a where
    newUArray_ :: (Ix i) => (i, i) -> ST s (STArray s i a)
    writeUArray :: (Ix i) => STArray s i a -> i -> a -> ST s ()

instance Discrete Float where
    newUArray_ = newArray_
    writeUArray = writeArray

instance Discrete Double where
    newUArray_ = newArray_
    writeUArray = writeArray

instance Discrete Int where
    newUArray_ = newArray_
    writeUArray = writeArray

