

module Drool.Utils.Transformation (
    fftwFloats, 
    signalIIR,
    iir, 
    applyIIR
) where 

import Debug.Trace

import Drool.Utils.Conversions as Conv
-- import Numeric.FFT as FFT

import Math.FFT as FFTW
import Drool.Types as DT ( Signal(..), SignalList(..) )
import Drool.Utils.Conversions as Conv
import GHC.Float
import Data.Array.CArray ( size ) 
import Data.Array.IO ( IOUArray, getElems, newListArray, getBounds )
import Foreign.Marshal.Array ( peekArray )

{-
fftFloats :: [Float] -> [Float]
fftFloats fs = Conv.complexDoublesToFloats $ FFT.fft (Conv.floatsToComplexDoubles fs)
-}

fftwFloats :: [Float] -> IO [Float]
fftwFloats fs = ( do 
  -- [Float] to to [Double] to CArray Integer Double:
  carrDoubles <- Conv.listToCArray $ (map ( \f -> float2Double f ) fs)
  -- CArray Integer Float to CArray Integer (Complex Float), 
  -- carrCDoubles now containts the DFT result: 
  let fftCDoubles = FFTW.dftRC carrDoubles
  -- Unmarshal array to [Complex Double]: 
  let lstCDoubles = Conv.listFromCArray fftCDoubles
  -- Convert [Complex Double] to [Float]: 
  return $ Conv.complexDoublesToFloats lstCDoubles )

signalSampleIIR :: DT.Signal -> DT.Signal -> Float -> IO (DT.Signal)
signalSampleIIR sigVsPrev sigVs coef = do sigVsPrev' <- getElems $ DT.signalArray sigVsPrev
                                          sigVs'     <- getElems $ DT.signalArray sigVs
                                          aRange     <- getBounds $ DT.signalArray sigVs
                                          filteredSignal <- (newListArray aRange $ zipWith ( \psv sv -> sv * coef + psv * (1-coef) ) sigVsPrev' sigVs') :: IO (IOUArray Int Float)
                                          return (DT.CSignal filteredSignal)

signalIIR :: DT.SignalList -> Int -> Float -> IO (DT.SignalList)
signalIIR sigList num coef = do updatedSignals <- (signalIIR' sigList num coef) 
                                return $ DT.CSignalList ((DT.signalList updatedSignals) ++ (drop num (DT.signalList sigList)))

signalIIR' :: DT.SignalList -> Int -> Float -> IO (DT.SignalList)
signalIIR' sigList@(DT.CSignalList (_:_:_)) num coef = do let sigs        = DT.signalList sigList
                                                          let newSigs     = (reverse (take (num+1) sigs)) -- [ old, new, new, ...]
                                                          let lastOldSig  = (newSigs !! 0) :: DT.Signal
                                                          let sigToUpdate = (newSigs !! 1) :: DT.Signal
                                                          recursedSignals <- if num > 1 then (signalIIR' sigList (num-1) coef) else return (DT.CSignalList [])
                                                          filteredSamples <- signalSampleIIR lastOldSig sigToUpdate coef 
                                                          return $ DT.CSignalList ( (DT.signalList recursedSignals) ++ [ filteredSamples ] )
signalIIR' sigList@(DT.CSignalList (_:[])) _ _ = do return sigList
signalIIR' sigList@(DT.CSignalList []) _ _ = do return sigList


iir :: (Fractional a) => a -> [a] -> [a] -> a
iir sample samples coefs = foldl (\ac (i,v) -> ac + (coefs !! i) * v) 0.0 (zip [0..num-1] (sample : (take num samples)))
  where num = length coefs

-- Applies IIR filter to buffer, updating num elements in the front. 
-- Element in the front is considered the most recent signal sample. 
applyIIR :: (Fractional a) => [a] -> [a] -> Int -> [a]
applyIIR samples coefs num =  if num > 0 then applyIIR (headSamples ++ [iirSample] ++ tailSamples) coefs (num-1) else samples
  where iirSample = iir (samples !! (num - 1)) tailSamples coefs
        headSamples = take (num-1) samples
        tailSamples = drop num samples

