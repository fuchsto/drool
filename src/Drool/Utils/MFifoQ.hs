{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE TypeFamilies, FlexibleContexts, GADTs #-}

-- Source: http://github.com/Peaker/slidingmax

module Drool.Utils.MFifoQ(
    MFifoQ, MFifoQOf, new, new', readAt, writeAt, append,
    popFirst, truncate, elems, length
)
where

import Prelude hiding (length, truncate)
import Control.Monad(liftM2, join)
import Control.Monad.ST(ST)
import Data.Array.IO(IOArray)

import Data.Array.ST(STArray)
import Data.Array.MArray(MArray, newArray_, readArray, writeArray, getBounds)

import Data.IORef(IORef, newIORef, readIORef, writeIORef)

import Data.STRef(STRef, newSTRef, readSTRef, writeSTRef)


class PolyRef r where
  type RefMonad r :: * -> *
  newPolyRef :: a -> RefMonad r (r a)
  writePolyRef :: r a -> a -> RefMonad r ()
  readPolyRef :: r a -> RefMonad r a

{-- IO --}

instance PolyRef IORef where
  type RefMonad IORef = IO
  newPolyRef = newIORef
  writePolyRef = writeIORef
  readPolyRef = readIORef

{-- ST --}

instance PolyRef (STRef s) where
  type RefMonad (STRef s) = ST s
  newPolyRef = newSTRef
  writePolyRef = writeSTRef
  readPolyRef = readSTRef



type family MFifoQOf (m :: * -> *) a

type instance MFifoQOf IO a = MFifoQ IOArray IORef a
type instance MFifoQOf (ST s) a = MFifoQ (STArray s) (STRef s) a

data MFifoQ arr ref a where
    MFifoQ :: (MArray arr a m, m ~ RefMonad ref, PolyRef ref) =>
              arr Int a -> ref Int -> ref Int -> MFifoQ arr ref a

new :: (PolyRef ref, MArray arr a (RefMonad ref)) =>
       Int -> RefMonad ref (MFifoQ arr ref a)
new i = do
  arr <- newArray_ (0, i-1)
  hd <- newPolyRef 0
  count <- newPolyRef 0
  return $ MFifoQ arr hd count

new' :: (MFifoQOf m a ~ MFifoQ arr ref a,
         m ~ RefMonad ref,
         PolyRef ref, MArray arr a m) =>
        Int -> m (MFifoQOf m a)
new' = new

-- Only read/mutate array
inFQ_ :: MFifoQ arr ref a ->
         (arr Int a -> Int -> (Int, Int) ->
          RefMonad ref b) ->
         RefMonad ref b
inFQ_ (MFifoQ arr hd cnt) func = do
  (0, high) <- getBounds arr
  join $ liftM2 (curry (func arr (high+1)))
           (readPolyRef hd) (readPolyRef cnt)

-- Read/mutate array AND the position indices
inFQ :: MFifoQ arr ref a ->
        (arr Int a -> Int -> (Int, Int) ->
         RefMonad ref ((Int, Int), b)) ->
        RefMonad ref b
inFQ fifo@(MFifoQ _ hd cnt) func = do
  ((hd', cnt'), result) <- inFQ_ fifo func
  writePolyRef hd hd'
  writePolyRef cnt cnt'
  return result

readAt :: MFifoQ arr ref a -> Int -> RefMonad ref a
readAt fifo@(MFifoQ _ _ _) i = inFQ_ fifo doRead
  where
    doRead arr size (hd, cnt)
      | i >= cnt   =  fail "Invalid index"
      | otherwise  =  readArray arr ((hd + i) `mod` size)

writeAt :: MFifoQ arr ref a -> Int -> a -> RefMonad ref ()
writeAt fifo@(MFifoQ _ _ _) i x = inFQ_ fifo doWrite
  where
    doWrite arr size (hd, cnt)
      | i >= cnt   =  fail "Invalid index"
      | otherwise  =  writeArray arr ((hd + i) `mod` size) x

append :: MFifoQ arr ref a -> a -> RefMonad ref ()
append fifo@(MFifoQ _ _ _) x = inFQ fifo doWrite
  where
    doWrite arr size (hd, cnt)
      | cnt >= size  =  fail "Append too much"
      | otherwise    =  writeArray arr idx x >>
                        return ((hd, cnt+1), ())
      where
        idx = (hd + cnt) `mod` size

popFirst :: MFifoQ arr ref a -> RefMonad ref ()
popFirst fifo@(MFifoQ _ _ _) = inFQ fifo doPop
  where
    doPop _ size (hd, cnt)
      | size <= 0  =  fail "Pop empty queue"
      | otherwise  =  return ((hd+1, cnt-1), ())

truncate :: MFifoQ arr ref a -> Int -> RefMonad ref ()
truncate fifo@(MFifoQ _ _ _) l = inFQ fifo doTruncate
  where
    doTruncate _ _ (hd, cnt) = return ((hd, min l cnt), ())

elems :: MFifoQ arr ref a -> RefMonad ref [a]
elems fifo@(MFifoQ _ _ _) = inFQ_ fifo getElems
  where
    getElems arr size (hd, cnt) = mapM (readArray arr) (a ++ b)
      where
        a = [hd..min (hd+cnt) size - 1]
        b | hd+cnt-1 >= size = [0..hd+cnt-1 - size]
          | otherwise      = []

length :: MFifoQ arr ref a -> RefMonad ref Int
length fifo@(MFifoQ _ _ _) = inFQ_ fifo $ \_ _ (_, cnt) -> return cnt
