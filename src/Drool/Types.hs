-----------------------------------------------------------------------------
--
-- Module      :  Drool.Types
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

{-# OPTIONS -O2 -Wall #-}

module Drool.Types (
   RotationVector,
   ContextSettings(..),
   Signal(..),
   SignalBuffer(..),
   SignalList(..),
   RenderPerspective(..),
   rvector_x, rvector_y, rvector_z,
   newSignal,
   newSignalBuffer,
   newSignalList,
   getSignal,
   getBufferSample,
   getSignalSample
) where

import Data.Array.IO
import Graphics.Rendering.OpenGL
import Data.IORef

-- import Drool.Utils.MFifoQ

-- A signal is an array of samples (sample type is GLfloat):
newtype Signal = CSignal { signalArray :: IOArray Int GLfloat }

-- A signal buffer is an array of signals, signal[t] = signal_t
-- In effect, a two-dimensional matrix over samples.
newtype SignalBuffer = CSignalBuffer { signalBufferArray :: IOArray Int Signal }

-- Would perform better in case a list of Signal is too slow:
-- newtype SignalQueue = CSignalQueue { signalQueueArray :: MFifoQOf IO Signal }

newtype SignalList = CSignalList { signalList :: [Signal] }

-- Type for translation vector
-- newtype TVector = TVector (Vector3 GLfloat GLfloat GLfloat)
-- Type for rotation vector
type RotationVector = (GLfloat, GLfloat, GLfloat)

data RenderPerspective = Top | Left | Front | Isometric

-- Shared settings for communication between main controller, view options
-- and rendering:
data ContextSettings = ContextSettings { translation :: Vector3(GLfloat,GLfloat,GLfloat),
                                         rotation :: RotationVector,
                                         angle :: GLfloat,
                                         scaling :: GLfloat,
                                         gridOpacity :: GLfloat,
                                         surfaceOpacity :: GLfloat,
                                         renderPerspective :: RenderPerspective,
                                         signalBuf :: (IORef SignalList) }

rvector_x :: (a,a,a) -> a
rvector_x (x,_,_) = x

rvector_y :: (a,a,a) -> a
rvector_y (_,y,_) = y

rvector_z :: (a,a,a) -> a
rvector_z (_,_,z) = z

newSignal :: IO Signal
newSignal = fmap CSignal $ newArray(0,10) (0::GLfloat) :: IO Signal

newSignalBuffer :: Int -> IO SignalBuffer
newSignalBuffer size = do
  blankSignal <- newSignal
  fmap CSignalBuffer $ newArray(0,size) (blankSignal) :: IO SignalBuffer

newSignalList size el = if size > 0 then (el::Signal) : (newSignalList (size-1) el) else []

-- getSignal signalBuf time_idx = readArray (signalBufferArray signalBuf) time_idx
getSignal :: SignalList -> Int -> Signal
getSignal signals time_idx = (signalList signals) !! time_idx

{-
getBufferSample signalBuf time_idx sample_idx = do
  signal <- getSignal signalBuf time_idx
  sample <- readArray (signalArray signal) sample_idx
  return sample
-}

getBufferSample signals time_idx sample_idx = do
  let signal = getSignal signals time_idx
  sample <- readArray (signalArray signal) sample_idx
  return sample

getSignalSample signal sample_idx = do
  sample <- readArray (signalArray signal) sample_idx
  return sample

