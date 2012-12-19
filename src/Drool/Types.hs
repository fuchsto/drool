-----------------------------------------------------------------------------
--
-- Module      :  Drool.Types
-- Copyright   :  2012, Tobias Fuchs
-- License     :  MIT
--
-- Maintainer  : twh.fuchs@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- |
--
-----------------------------------------------------------------------------

{-# OPTIONS -O2 -Wall #-}

module Drool.Types (
   RotationVector(..),
   Signal(..),
-- SignalBuffer(..),
   SignalList(..),
   SignalSource(..),
   RenderPerspective(..),
   rvector_x, rvector_y, rvector_z,
   newSignal,
-- newSignalBuffer,
   newSignalList,
   getSignal,
   getRecentSignal, 
   getLastSignal, 
   getBufferSample,
   getSignalSample
) where

import Data.Array.IO
import Graphics.Rendering.OpenGL
import Drool.Utils.SigGen ( SValue )

-- A signal is an array of samples (sample type is GLfloat):
newtype Signal = CSignal { signalArray :: IOUArray Int Float }

-- A signal buffer is an array of signals, signal[t] = signal_t
-- In effect, a two-dimensional matrix over samples.
-- newtype SignalBuffer = CSignalBuffer { signalBufferArray :: IOArray Int Signal }

-- Would perform better in case a list of Signal is too slow:
-- newtype SignalQueue = CSignalQueue { signalQueueArray :: MFifoQOf IO Signal }

newtype SignalList = CSignalList { signalList :: [Signal] }

data SignalSource = Microphone | TestSignal | File
  deriving ( Eq, Show, Read )

-- Type for translation vector
-- newtype TVector = TVector (Vector3 GLfloat GLfloat GLfloat)
-- Type for rotation vector
data RotationVector = CRotationVector { rotX :: GLfloat, rotY :: GLfloat, rotZ :: GLfloat }
  deriving ( Show, Read ) 

data RenderPerspective = Top | Side | Front | Isometric
  deriving ( Show, Read )

rvector_x :: (a,a,a) -> a
rvector_x (x,_,_) = x

rvector_y :: (a,a,a) -> a
rvector_y (_,y,_) = y

rvector_z :: (a,a,a) -> a
rvector_z (_,_,z) = z

newSignal :: IO Signal
newSignal = fmap CSignal $ newArray(0,10) (0::Float) :: IO Signal

{-
newSignalBuffer :: Int -> IO SignalBuffer
newSignalBuffer size = do
  blankSignal <- newSignal
  fmap CSignalBuffer $ newArray(0,size) (blankSignal) :: IO SignalBuffer
-}

newSignalList :: Int -> Signal -> [Signal]
newSignalList size el = if size > 0 then (el::Signal) : (newSignalList (size-1) el) else []

-- getSignal signalBuf time_idx = readArray (signalBufferArray signalBuf) time_idx
getSignal :: SignalList -> Int -> Signal
getSignal signals time_idx = (signalList signals) !! time_idx

-- Using !! here is okay as signal buffer passed is supposed to be really short (<= 3 signals).
getRecentSignal :: SignalList -> Maybe Signal
getRecentSignal signals@(CSignalList (_:_)) = Just $ sigList !! 0
  where sigList = signalList signals
getRecentSignal (CSignalList []) = Nothing

getLastSignal :: SignalList -> Maybe Signal
getLastSignal signals@(CSignalList (_:_)) = Just $ last sigList 
  where sigList = signalList signals
getLastSignal (CSignalList []) = Nothing

{-
getBufferSample signalBuf time_idx sample_idx = do
  signal <- getSignal signalBuf time_idx
  sample <- readArray (signalArray signal) sample_idx
  return sample
-}

getBufferSample :: SignalList -> Int -> Int -> IO Float
getBufferSample signals time_idx sample_idx = do
  let signal = getSignal signals time_idx
  sample <- readArray (signalArray signal) sample_idx
  return sample

getSignalSample :: Signal -> Int -> IO SValue
getSignalSample signal sample_idx = do
  sample <- readArray (signalArray signal) sample_idx
  return sample



