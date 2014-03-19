{-# LANGUAGE FlexibleContexts, FlexibleInstances, TypeFamilies #-}

module Data.Function.ArrayMemoize where

import qualified Data.Array.MArray as MArray
import Data.Array.Unboxed
import Data.Array.IO (IOUArray)
import Data.Array.ST (STArray, STUArray, runSTArray)
import Control.Monad.ST

import Debug.Trace

-- Memoize a function as an array over a finite domain

discMemoFix :: (ArrayMemoizable b, Num a, Show (Discrete a), Show a, Discretize a) => ((a -> b) -> (a -> b)) -> (a, a) -> a -> (a -> b)
discMemoFix f (l, u) delta =
     let disc x = discretize x delta
         cache = runSTArray (do cache <- (newArray_ (disc l, disc u)) 
                                mapM_ (\x -> writeArray cache (disc x) (f' x)) (enumFromThenTo l (l+delta) u)
                                return cache)
         f' = f (\x -> (show (disc x)) `trace` cache ! (disc x))
     in f'

class (Ix (Discrete t), Enum t) => Discretize t where
    type Discrete t
    discretize :: t -> t -> Discrete t

instance Discretize Float where
    type Discrete Float = Int
    discretize x delta = floor (x / delta)

{-
instance Discretize Double where
    type Discrete Double = Int
    discretize x delta = floor (x / delta) -}

instance (Discretize a, Discretize b) => Discretize (a, b) where
    type Discrete (a, b) = (Discrete a, Discrete b)
    discretize (x, y) (dx, dy) = (discretize x dx, discretize y dy)

instance (Discretize a, Discretize b, Discretize c) => Discretize (a, b, c) where
    type Discrete (a, b, c) = (Discrete a, Discrete b, Discrete c)
    discretize (x, y, z) (dx, dy, dz) = (discretize x dx, discretize y dy, discretize z dz)




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

{-

Num and Enum classes for working with tuple domains

-}

instance (Enum a, Enum b) => Enum (a, b) where
    toEnum = undefined
    fromEnum (a, b) = fromEnum a * fromEnum b

    enumFromThenTo (lx, ly) (nx, ny) (ux, uy) = 
        [ly,ny..uy] >>= (\y -> [lx,nx..ux] >>= (\x -> return (x, y)))

instance (Enum a, Enum b, Enum c) => Enum (a, b, c) where
    toEnum = undefined
    fromEnum (a, b, c) = fromEnum a * fromEnum b * fromEnum c

    enumFromThenTo (lx, ly, lz) (nx, ny, nz) (ux, uy, uz) = 
        [lx,nx..ux] >>= (\x -> [ly,ny..uy] >>= (\y -> [lz,nz..uz] >>= (\z -> return (x, y, z))))

instance (Num a, Num b) => Num (a, b) where
    (a1, b1) + (a2, b2) = (a1 + a2, b1 + b2)
    (a1, b1) * (a2, b2) = (a1 * a2, b1 * b2)
    negate (a, b) = (negate a, negate b)
    abs (a, b) = (abs a, abs b)
    signum (a, b) = (signum a, signum b)
    fromInteger i = (0, fromInteger i)
