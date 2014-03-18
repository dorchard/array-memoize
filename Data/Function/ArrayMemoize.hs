{-# LANGUAGE FlexibleContexts #-}

module Data.Function.ArrayMemoize where

import qualified Data.Array.MArray as MArray
import Data.Array.Unboxed
import Data.Array.IO (IOUArray)
import Data.Array.ST (STArray, STUArray, runSTArray)
import Control.Monad.ST

-- Memoize a function as an array over a finite domain

arrayMemoFix :: (Ix a, ArrayMemoizable b) => ((a -> b) -> (a -> b)) -> (a, a) -> a -> b
arrayMemoFix f (l, u) =
     let cache = runSTArray (do cache <- (newArray_ (l, u)) 
                                mapM_ (\x -> writeArray cache x (f' x)) (range (l, u))
                                return cache)
         f' = f (\x -> cache ! x)
     in f'

-- Use an unboxed IO array instead (but requires incoming function must return IO

uarrayMemoFixIO :: (Ix a, UArrayMemoizable b) => ((a -> IO b) -> (a -> IO b)) -> (a, a) -> a -> IO b
uarrayMemoFixIO f (l, u) =
    \i -> do cache <- (newUArray_ (l, u))
             let f' = f (\x -> readUArray cache x)
             mapM_ (\x -> (f' x) >>= (\val -> writeUArray cache x val)) (range (l, u))            
             f' i

{-

 The following defines the subset of types for which we can do array 
 memoization

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

-- Unboxed versions using IO

class IArray UArray a => UArrayMemoizable a where
    newUArray_ :: (Ix i) => (i, i) -> IO (IOUArray i a)
    writeUArray :: (Ix i) => IOUArray i a -> i -> a -> IO ()
    readUArray :: (Ix i) => IOUArray i a -> i -> IO a
    freeze :: (Ix i) => IOUArray i a -> IO (UArray i a)

instance UArrayMemoizable Float where
    newUArray_ = MArray.newArray_
    readUArray = MArray.readArray
    writeUArray = MArray.writeArray
    freeze = MArray.freeze

instance UArrayMemoizable Double where
    newUArray_ = MArray.newArray_
    readUArray = MArray.readArray
    writeUArray = MArray.writeArray
    freeze = MArray.freeze

instance UArrayMemoizable Int where
    newUArray_ = MArray.newArray_
    readUArray = MArray.readArray
    writeUArray = MArray.writeArray
    freeze = MArray.freeze

