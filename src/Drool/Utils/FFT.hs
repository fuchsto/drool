

module Drool.Utils.FFT (
    fftFloats
) where 

import Drool.Utils.Conversions as Conv
import Numeric.FFT 

fftFloats :: [Float] -> [Float]
fftFloats fs = Conv.complexDoublesToFloats $ fft (Conv.floatsToComplexDoubles fs)

