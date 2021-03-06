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
    gtkColorToGLColor3,
    gtkColorToGLColor4,
    glColor3ToGtkColor,
    glColor4ToGtkColor,
    freqToMs,
    msToFreq, 
    floatToComplexDouble, 
    floatsToComplexDoubles, 
    complexDoubleToFloat, 
    complexDoublesToFloats, 
    listToCArray, 
    listFromCArray, 
    interleave, 
    interleaveArrays, 
    aMax, 
    aLength, 
    aZip, 
    adjustBufferSize,
    adjustBufferSizeBack, 
    blendModeSourceFromIndex, 
    blendModeFrameBufferFromIndex, 
    blendModeSourceIndex, 
    blendModeFrameBufferIndex, 
) where

import qualified Graphics.UI.Gtk as Gtk
import Graphics.Rendering.OpenGL
import Data.Complex
import Data.Array ( Array )
import Data.Array.IArray ( (!), bounds, listArray, ixmap, amap, rangeSize )
import Data.Array.CArray ( createCArray, elems )
import Foreign.Marshal.Array ( pokeArray, peekArray )
import GHC.Float
import Data.Word ( Word16 )

gtkColorToGLColor3 :: Gtk.Color -> Color3 GLfloat
gtkColorToGLColor3 (Gtk.Color r g b) = Color3 r' g' b'
  where r' = ((fromIntegral r) / 65535.0) :: GLfloat
        g' = ((fromIntegral g) / 65535.0) :: GLfloat
        b' = ((fromIntegral b) / 65535.0) :: GLfloat

gtkColorToGLColor4 :: Gtk.Color -> Word16 -> Color4 GLfloat
gtkColorToGLColor4 (Gtk.Color r g b) a = Color4 r' g' b' a'
  where r' = ((fromIntegral r) / 65535.0) :: GLfloat
        g' = ((fromIntegral g) / 65535.0) :: GLfloat
        b' = ((fromIntegral b) / 65535.0) :: GLfloat
        a' = ((fromIntegral a) / 65535.0) :: GLfloat

glColor3ToGtkColor :: Color3 GLfloat -> Gtk.Color
glColor3ToGtkColor (Color3 r g b) = Gtk.Color r' g' b'
  where r' = round(r * 65535.0)
        g' = round(g * 65535.0)
        b' = round(b * 65535.0)

glColor4ToGtkColor :: Color4 GLfloat -> ( Gtk.Color, Word16 )
glColor4ToGtkColor (Color4 r g b a) = (Gtk.Color r' g' b', a')
  where r' = round(r * 65535.0)
        g' = round(g * 65535.0)
        b' = round(b * 65535.0)
        a' = round(a * 65535.0)

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
-- complexDoubleToFloat cd = realToFrac(sqrt (realPart cd * realPart cd)) :: Float

complexDoublesToFloats :: [Complex Double] -> [Float]
complexDoublesToFloats cds = map (\x -> (complexDoubleToFloat x) / n) cds
  where n = fromIntegral $ length cds

listToCArray lst = createCArray (0,(length lst)-1) ( \ptr -> do { pokeArray ptr lst } )

listFromCArray carr = elems carr

interleave :: [a] -> [a] -> [a]
interleave xsA xsB = foldl (\acc (a,b) -> acc ++ [a,b] ) [] (zip xsA xsB)

aLength a = rangeSize $ bounds a

aMax a = snd $ bounds a

interleaveArrays :: Array Int e -> Array Int e -> Array Int e 
interleaveArrays a b = listArray (0,(aLength a) + (aMax b)) (concat [ [(a ! i),(b ! i)] | i <- [0..(min (aMax a) (aMax b))] ])

aZip a b = [ ((a ! i),(b ! i)) | i <- [0..(min (aMax a) (aMax b))] ]

adjustBufferSize :: [a] -> Int -> [a] 
adjustBufferSize buf maxLen = drop ((length buf)-maxLen) buf -- drop does not alter list for values <= 0

adjustBufferSizeBack :: [a] -> Int -> [a] 
adjustBufferSizeBack buf maxLen = take maxLen buf -- drop does not alter list for values <= 0

blendSourceModes :: [ BlendingFactor ]
blendSourceModes = [ Zero, One, DstColor, OneMinusDstColor, SrcAlpha, 
                     OneMinusSrcAlpha, DstAlpha, OneMinusDstAlpha, 
                     SrcAlphaSaturate ] 
blendFrameBufferModes :: [ BlendingFactor ]
blendFrameBufferModes = [ Zero, One, SrcColor, OneMinusSrcColor, SrcAlpha, 
                          OneMinusSrcAlpha, DstAlpha, OneMinusDstAlpha ] 

blendModeSourceFromIndex :: Int -> BlendingFactor
blendModeSourceFromIndex i = blendSourceModes !! i

blendModeFrameBufferFromIndex :: Int -> BlendingFactor
blendModeFrameBufferFromIndex i = blendFrameBufferModes !! i

blendModeSourceIndex :: BlendingFactor -> Int
blendModeSourceIndex bm = blendModeSourceIndex' bm 0 blendSourceModes
blendModeSourceIndex' :: BlendingFactor -> Int -> [ BlendingFactor ] -> Int
blendModeSourceIndex' bm i (mode:modes) = index
  where index = if bm == mode then i else blendModeSourceIndex' bm (i+1) modes
blendModeSourceIndex' _ _ [] = 0

blendModeFrameBufferIndex :: BlendingFactor -> Int
blendModeFrameBufferIndex bm = blendModeFrameBufferIndex' bm 0 blendFrameBufferModes
blendModeFrameBufferIndex' :: BlendingFactor -> Int -> [ BlendingFactor ] -> Int
blendModeFrameBufferIndex' bm i (mode:modes) = index
  where index = if bm == mode then i else blendModeFrameBufferIndex' bm (i+1) modes
blendModeFrameBufferIndex' _ _ [] = 0
