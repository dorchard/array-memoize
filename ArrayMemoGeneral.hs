{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, RankNTypes, ExistentialQuantification, GADTs, TypeOperators, KindSignatures, MultiParamTypeClasses, ConstraintKinds #-}

import Data.Array.MArray
import Data.Array.Unboxed
import Data.Array.ST
import Control.Monad.ST

import Data.Constraint (Constraint, (:-), (\\))
import Data.Constraint.Forall 

data Predicate (a :: * -> Constraint) = Predicate


class (MArray (STUArray s) e (ST s)) => Foo e s

data ArrayMemoFun a b where
    Fun :: forall a b s . ((a -> b) -> (a -> b)) -> ArrayMemoFun a b

data ArrayFun a b where
    Fun2 :: (Forall (Foo b)) => (a -> b) -> ArrayFun a b


memArr :: forall a b . (Ix a, IArray UArray b, Forall (Foo b)) =>
           (ArrayMemoFun a b) -- unfixed function
        -> (a, a)             -- bounds
        -> ArrayFun a b           -- memoized fixed function
memArr (Fun (f :: forall s . ((a -> b) -> (a -> b)))) (l, u) = 
      (let

        cache = (runSTUArray (cacheM (l, u) f')) 
        f' = f (\x -> cache ! x)

       in (Fun2 f')) \\ (inst_ (Predicate :: Predicate (Foo b)) (proxy (undefined::s)))
 

{- 
grr :: (Ix i, Foo e s) => (i -> e) -> (i, i) -> (ST s) (STUArray s i e)
grr f' (l, u) = do cache <- (newArray_ (l, u))
                   mapM_ (\x -> writeArray cache x (f' x)) (range (l, u))
                   return cache


data Predicate (a :: * -> Constraint) = Predicate

foo :: forall i e . (Ix i, Forall (Foo e)) => (i -> e) -> (i, i) -> UArray i e
foo f' (l, u) = (runSTUArray (grr f' (l, u))) \\ (inst_ (Predicate :: Predicate (Foo e)))



-} 

data Proxy (a :: *) = Proxy
proxy :: a -> Proxy a
proxy _ = Proxy

cacheM :: (Ix a, MArray (STUArray s) b (ST s)) => (a, a) -> (a -> b) -> ST s (STUArray s a b)
cacheM (l, u) f' = do cache <- (newArray_ (l, u))
                      mapM_ (\x -> writeArray cache x (f' x)) (range (l, u))
                      return cache

inst_ :: Predicate p -> Proxy a -> Forall p :- p a
inst_ _ _ = inst