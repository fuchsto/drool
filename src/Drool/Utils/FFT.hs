

module Drool.Utils.FFT (
    fftFloats, 
    fftwFloats
) where 

import Drool.Utils.Conversions as Conv
import Numeric.FFT as FFT

import Math.FFT as FFTW
import Drool.Utils.Conversions as Conv
import GHC.Float
import Data.Array.CArray ( size ) 
import Foreign.Marshal.Array ( peekArray )

fftFloats :: [Float] -> [Float]
fftFloats fs = Conv.complexDoublesToFloats $ FFT.fft (Conv.floatsToComplexDoubles fs)

fftwFloats :: [Float] -> IO [Float]
fftwFloats fs = ( do 
  -- let numSamples = length fs
  -- [Float] to to [Double] to CArray Integer Double:
  carrDoubles <- Conv.listToCArray $ (map ( \f -> float2Double f ) fs)
  -- CArray Integer Float to CArray Integer (Complex Float), 
  -- carrCDoubles now containts the DFT result: 
  let fftCDoubles = FFTW.dftRC carrDoubles
  -- Unmarshal array to [Complex Double]: 
  let numSamples = size fftCDoubles
  let lstCDoubles = Conv.listFromCArray fftCDoubles
  -- Convert [Complex Double] to [Float]: 
  return $ Conv.complexDoublesToFloats lstCDoubles )
