-----------------------------------------------------------------------------
--
-- Module      :  Drool.Utils.Conversions
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

module Drool.Utils.Conversions (
    gtkColorToGLColor,
    glColorToGtkColor,
    freqToMs,
    msToFreq, 
    floatToComplexDouble, 
    floatsToComplexDoubles, 
    complexDoubleToFloat, 
    complexDoublesToFloats, 
    listToCArray, 
    listFromCArray, 
    interleave, 
    adjustBufferSize,
    adjustBufferSizeBack
) where

import qualified Graphics.UI.Gtk as Gtk
import Graphics.Rendering.OpenGL
import Data.Complex
import Data.Array.CArray ( createCArray, elems )
import Foreign.Marshal.Array ( pokeArray, peekArray )
import GHC.Float

gtkColorToGLColor :: Gtk.Color -> Color3 GLfloat
gtkColorToGLColor (Gtk.Color r g b) = Color3 r' g' b'
  where r' = ((fromIntegral r) / 65535.0) :: GLfloat
        g' = ((fromIntegral g) / 65535.0) :: GLfloat
        b' = ((fromIntegral b) / 65535.0) :: GLfloat

glColorToGtkColor :: Color3 GLfloat -> Gtk.Color
glColorToGtkColor (Color3 r g b) = Gtk.Color r' g' b'
  where r' = round(r * 65535.0)
        g' = round(g * 65535.0)
        b' = round(b * 65535.0)


freqToMs :: Int -> Int
freqToMs f = round(1000.0 / fromIntegral f)

msToFreq :: Int -> Int
msToFreq ms = round(1000.0 / fromIntegral ms)

floatToComplexDouble :: Float -> Complex Double
floatToComplexDouble f = (float2Double f :+ 0.0) :: Complex Double

floatsToComplexDoubles :: [Float] -> [Complex Double]
floatsToComplexDoubles fs = (map (\x -> floatToComplexDouble x) fs)

-- Returns magnitude of complex double as float, with magnitude = (re^2 + im^2)^0.5
complexDoubleToFloat :: Complex Double -> Float
complexDoubleToFloat cd = realToFrac(sqrt (realPart cd * realPart cd + imagPart cd * imagPart cd)) :: Float

complexDoublesToFloats :: [Complex Double] -> [Float]
complexDoublesToFloats cds = map (\x -> (complexDoubleToFloat x) / 100.0) cds

listToCArray lst = createCArray (0,(length lst)-1) ( \ptr -> do { pokeArray ptr lst } )

listFromCArray carr = elems carr

interleave :: [a] -> [a] -> [a]
interleave xsA xsB = foldl (\acc (a,b) -> acc ++ [a,b] ) [] (zip xsA xsB)

adjustBufferSize :: [a] -> Int -> [a] 
adjustBufferSize buf maxLen = drop ((length buf)-maxLen) buf -- drop does not alter list for values <= 0

adjustBufferSizeBack :: [a] -> Int -> [a] 
adjustBufferSizeBack buf maxLen = take maxLen buf -- drop does not alter list for values <= 0

