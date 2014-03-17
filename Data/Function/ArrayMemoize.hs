{-# LANGUAGE FlexibleContexts #-}

module Data.Function.ArrayMemoize

import Data.Array.MArray
import Data.Array.Unboxed
import Data.Array.ST
import Control.Monad.ST

-- Memoize a function as an array over a finite domain

arrayMemo :: (Ix a, ArrayMemo b) => ((a -> b) -> (a -> b)) -> (a, a) -> a -> b
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
 
class IArray UArray a => ArrayMemo a where
    newUArray_ :: (Ix i) => (i, i) -> ST s (STArray s i a)
    readUArray :: (Ix i) => STArray s i a -> i -> ST s a
    writeUArray :: (Ix i) => STArray s i a -> i -> a -> ST s ()

instance ArrayMemo Float where
    newUArray_ = newArray_
    readUArray = readArray
    writeUArray = writeArray

instance ArrayMemo Double where
    newUArray_ = newArray_
    readUArray = readArray
    writeUArray = writeArray

instance ArrayMemo Int where
    newUArray_ = newArray_
    readUArray = readArray
    writeUArray = writeArray

