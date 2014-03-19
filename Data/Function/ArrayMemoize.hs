{-# LANGUAGE FlexibleContexts, FlexibleInstances, TypeFamilies #-}

module Data.Function.ArrayMemoize where

import qualified Data.Array.MArray as MArray
import Data.Array.Unboxed
import Data.Array.IO (IOUArray)
import Data.Array.ST (STArray, STUArray, runSTArray)
import Control.Monad.ST

import Debug.Trace

{-

Attempt at rewrite rules

{-# RULES  "disc-rw1" continuize = cont';
           "disc-rw2" discretize = disc';
           "disc1a" forall delta x . disc' delta (cont' delta x) = x; 
           "disc2a" forall delta x . cont' delta (disc' delta x) = x; 
           "disc1b" forall delta . (disc' delta) . (cont' delta) = id;
  #-}

{-# NOINLINE cont' #-}
cont' = continuize 
{-# NOINLINE disc' #-}
disc' = discretize 

-}

-- Memoize and quantize a function over a finite (sub)domain, using an array. 

{-# INLINE quantizedMemo #-}
quantizedMemo :: (ArrayMemoizable b, Discretize a) => (a -> b) -> (a, a) -> a -> (a -> b)
quantizedMemo f (l, u) delta =
     let disc  = discretize delta
         cache = runSTArray (do cache <- newArray_ (disc l, disc u)
                                mapM_ (\x -> writeArray cache x (f (continuize delta x))) (enumFromTo (disc l) (disc u))
                                return cache)
     in (\x -> cache ! disc x)

{-# INLINE quantizedMemoFix #-}
quantizedMemoFix :: (ArrayMemoizable b, Discretize a) => ((a -> b) -> (a -> b)) -> (a, a) -> a -> (a -> b)
quantizedMemoFix f (l, u) delta = memo_f where memo_f = quantizedMemo (f memo_f) (l, u) delta 

-- Memoize and discretize a function over a finite (sub)domain, using an array. 

{-# INLINE discreteMemo #-}
discreteMemo :: (ArrayMemoizable b, Discretize a) => (a -> b) -> (a, a) -> a -> (Discrete a -> b)
discreteMemo f (l, u) delta =
     let disc  = discretize delta
         cache = runSTArray (do cache <- newArray_ (disc l, disc u)
                                mapM_ (\x -> writeArray cache x (f (continuize delta x))) (enumFromTo (disc l) (disc u))
                                return cache)
         
     in (\x -> cache ! x)

{-# INLINE discreteMemoFix #-}
discreteMemoFix :: (ArrayMemoizable b, Discretize a) => ((a -> b) -> (a -> b)) -> (a, a) -> a -> (Discrete a -> b)
discreteMemoFix f (l, u) delta =
     let disc  = discretize delta
         cache' = runSTArray (do cache <- newArray_ (disc l, disc u)
                                 mapM_ (\x -> writeArray cache x (f (\x -> cache' ! (disc x)) (continuize delta x))) (enumFromTo (disc l) (disc u))
                                 return cache)
     in (\x -> cache' ! x)

-- Memoize a function over a finite (sub)domain, using an array. 

{-# INLINE arrayMemo #-}
arrayMemo :: (Ix a, ArrayMemoizable b) => (a -> b) -> (a, a) -> (a -> b)
arrayMemo f (l, u) = 
    let cache = runSTArray (do cache <- newArray_ (l, u)
                               mapM_ (\x -> writeArray cache x (f x)) (range (l, u))
                               return cache)
    in \x -> cache ! x

{-# INLINE arrayMemoFix #-}
arrayMemoFix :: (Ix a, ArrayMemoizable b) => ((a -> b) -> (a -> b)) -> (a, a) -> a -> b
arrayMemoFix f (l, u) = memo_f where memo_f = arrayMemo (f memo_f) (l, u)

-- Memoize an function over a finite (sub)domain, using an unboxed IO array
--       requires incoming function must return IO - but be otherwise pure

{-# INLINE uarrayMemoFixIO #-}
uarrayMemoFixIO :: (Ix a, UArrayMemoizable b) => ((a -> IO b) -> (a -> IO b)) -> (a, a) -> a -> IO b
uarrayMemoFixIO f (l, u) =
    \i -> do cache <- newUArray_ (l, u)
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

    enumFromTo (lx, ly) (ux, uy) = 
        [ly..uy] >>= (\y -> [lx..ux] >>= (\x -> return (x, y)))

instance (Enum a, Enum b, Enum c) => Enum (a, b, c) where
    toEnum = undefined
    fromEnum (a, b, c) = fromEnum a * fromEnum b * fromEnum c

    enumFromThenTo (lx, ly, lz) (nx, ny, nz) (ux, uy, uz) = 
        [lz,nz..uz] >>= (\z -> [ly,ny..uy] >>= (\y -> [lx,nx..ux] >>= (\x -> return (x, y, z))))

instance (Num a, Num b) => Num (a, b) where
    (a1, b1) + (a2, b2) = (a1 + a2, b1 + b2)
    (a1, b1) * (a2, b2) = (a1 * a2, b1 * b2)
    negate (a, b) = (negate a, negate b)
    abs (a, b) = (abs a, abs b)
    signum (a, b) = (signum a, signum b)
    fromInteger i = (0, fromInteger i)


instance (Num a, Num b, Num c) => Num (a, b, c) where
    (a1, b1, c1) + (a2, b2, c2) = (a1 + a2, b1 + b2, c1 + c2)
    (a1, b1, c1) * (a2, b2, c2) = (a1 * a2, b1 * b2, c1 * c2)
    negate (a, b, c) = (negate a, negate b, negate c)
    abs (a, b, c) = (abs a, abs b, abs c)
    signum (a, b, c) = (signum a, signum b, signum c)
    fromInteger i = (0, 0, fromInteger i)

{-  Discretization of float/double values and tuples -}

class (Ix (Discrete t), Enum (Discrete t)) => Discretize t where
    type Discrete t
    discretize :: t -> t -> Discrete t
    continuize :: t -> Discrete t -> t

instance Discretize Float where
    type Discrete Float = Int
    discretize delta x = round' (x / delta) 
                           where round' x = let (n,r) = properFraction x in n + (round r)
    continuize delta x = (fromIntegral x) * delta

instance Discretize Double where
    type Discrete Double = Int
    discretize delta x = round' (x / delta) 
                           where round' x = let (n,r) = properFraction x in n + (round r)
    continuize delta x = (fromIntegral x) * delta

instance (Discretize a, Discretize b) => Discretize (a, b) where
    type Discrete (a, b) = (Discrete a, Discrete b)
    discretize (dx, dy) (x, y) = (discretize dx x, discretize dy y)
    continuize (dx, dy) (x, y) = (continuize dx x, continuize dy y)

instance (Discretize a, Discretize b, Discretize c) => Discretize (a, b, c) where
    type Discrete (a, b, c) = (Discrete a, Discrete b, Discrete c)
    discretize (dx, dy, dz) (x, y, z) = (discretize dx x, discretize dy y, discretize dz z)
    continuize (dx, dy, dz) (x, y, z) = (continuize dx x, continuize dy y, continuize dz z)

discrete :: Discretize a => (a -> b) -> a -> (Discrete a -> b)
discrete f delta = f . (continuize delta)