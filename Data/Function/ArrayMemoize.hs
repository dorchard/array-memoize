{-# LANGUAGE FlexibleContexts #-}

module Data.Function.ArrayMemoize where

import qualified Data.Array.MArray as MArray
import Data.Array.Unboxed
import Data.Array.ST (STArray, STUArray, runSTArray)
import Control.Monad.ST

-- Memoize a function as an array over a finite domain

arrayMemo :: (Ix a, ArrayMemoizable b) => ((a -> b) -> (a -> b)) -> (a, a) -> a -> b
arrayMemo f (l, u) =
     let cache = runSTArray (do cache <- (newArray_ (l, u)) 
                                mapM_ (\x -> writeArray cache x (f' x)) (range (l, u))
                                return cache)
         f' = f (\x -> cache ! x)
     in f'


{-

 The following defines the subset of types for which we can do array 
 memoization (the unboxed class). 

-}
 
class ArrayMemoizable a where
    newArray_ :: (Ix i) => (i, i) -> ST s (STArray s i a)
    writeArray :: (Ix i) => STArray s i a -> i -> a -> ST s ()

instance ArrayMemoizable Float where
    newArray_ = MArray.newArray_
    writeArray = MArray.writeArray

instance ArrayMemoizable Double where
    newArray_ = MArray.newArray_
    writeArray = MArray.writeArray

instance ArrayMemoizable Int where
    newArray_ = MArray.newArray_
    writeArray = MArray.writeArray

