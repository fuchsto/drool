-----------------------------------------------------------------------------
--
-- Module      :  Drool.Utils.SigGen
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

module Drool.Utils.SigGen (
  square, dirac, sine, saw,

  SValue, TValue,
  BaseSignal(..),
  AmpTransformation(..),
  SignalGenerator(..),

  genSignal
) where

import Control.Monad.State


type SValue = Float
type TValue = Int


-- Generates a dirac impulse of amplitude 1 in every period.
dirac :: Int -> TValue -> SValue
dirac periodlength t = if t `mod` periodlength == 0 then fromIntegral 1 else fromIntegral 0

-- Generates a square signal of amplitude (0,1) and given period length.
square :: Int -> TValue -> SValue
square periodlength t = if phase < wl'/2.0 then fromIntegral 1 else fromIntegral 0
  where phase = fromIntegral(t `mod` periodlength)
        wl'   = fromIntegral(periodlength)

-- Generates a sine signal of amplitude (-1..1) and given period length.
sine :: Int -> TValue -> SValue
sine periodlength t = sin (t'*2*pi/pl')
  where t'  = fromIntegral t
        pl' = fromIntegral periodlength

-- Generates a saw tooth signal of amplitude (0..1) and given period length.
saw :: Int -> TValue -> SValue
saw periodlength t = phase
  where phase = fromIntegral(t `mod` periodlength) / fromIntegral(periodlength)

-- A base signal is a function expecting t and period length, returning a sample value for t:
newtype BaseSignal = CBaseSignal { baseSignalFun :: (Int -> TValue -> SValue) }
-- An amplitude transformation is a function expecting t and a base signal sample,
-- transforming it's amplitute:
newtype AmpTransformation = CAmpTransformation { ampTransformationFun :: (Int -> TValue -> SValue -> SValue) }

-- A signal generator consists of a base signal and an amplitude transformation:
data SignalGenerator = CSignalGenerator { ampTransformation :: AmpTransformation,
                                          baseSignal :: BaseSignal,
                                          numSamples :: Int,
                                          signalPeriodLength :: Int,
                                          transPeriodLength :: Int }

-- A signal at time t is generated by retreiving the base signal value at time t and applying
-- the amplitude transformation contained in a signal generator.
genSignal :: SignalGenerator -> TValue -> [SValue]
genSignal siggen t = [ ampFun tpLength t (sigFun spLength x) | x <- [0..nSamples] ]
  where sigFun   = baseSignalFun $ baseSignal siggen
        spLength = signalPeriodLength siggen
        tpLength = transPeriodLength siggen
        ampFun   = ampTransformationFun $ ampTransformation siggen
        nSamples = numSamples siggen



