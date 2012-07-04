

module Drool.Utils.RenderHelpers (
    applyBandRangeAmp, 
    bandRangeAmpSamples, 

    vertexWithNormal, 
    verticesAndNormalsFromSignalList, 
    getVertexAndNormal
) where

import Data.Array.IO ( readArray )
import Data.Array.MArray ( getBounds )
import Data.Ix ( rangeSize )
import Drool.Types ( SignalList(..), Signal(..) )
import Drool.Utils.SigGen ( SValue, TValue )
import Graphics.Rendering.OpenGL ( 
    Vector3 (..), 
    Vertex3 (..), 
    Normal3 (..), 
    VertexComponent,
    NormalComponent, 
    vertex, normal, GLfloat )


-- Expects a sample, t, number of samples in total, list of band range amplifiers, and returns amplified sample for t. 
applyBandRangeAmp :: SValue -> TValue -> Int -> [Float] -> SValue
applyBandRangeAmp s t numSamples amps = s * ampValue
  where numRanges      = length amps
        rangeWidth     = numSamples `div` numRanges                          -- ...[-------]...
        rangePos       = t `mod` rangeWidth                                  -- ...|....x..|...
        activeRangeIdx = ((fromIntegral t)-rangePos) `div` rangeWidth :: Int -- [.|x|.|.|.]
    --  amp_1          = realToFrac $ amps !! (max 0 (activeRangeIdx-1))             -- Amp value of previous range, use range 0 as previous range of range 0
        amp_1          = realToFrac $ amps !! activeRangeIdx                         -- Amp value of active range
        amp_2          = realToFrac $ amps !! (min (activeRangeIdx+1) (numRanges-1)) -- Amp value of next range, use range n as next range of range n
        ampValue       = amp_1 + ((amp_2-amp_1) * (fromIntegral (rangePos) / fromIntegral (rangeWidth-1)))
        
bandRangeAmpSamples :: [SValue] -> [Float] -> [SValue]
bandRangeAmpSamples samples amps = bandRangeAmpSamplesRec samples amps (length samples) 0

bandRangeAmpSamplesRec :: [SValue] -> [Float] -> Int -> TValue -> [SValue]
bandRangeAmpSamplesRec (x:xs) amps numSamples t = ampSample : (bandRangeAmpSamplesRec xs amps numSamples (t+1))
  where ampSample = (applyBandRangeAmp x t numSamples amps)
bandRangeAmpSamplesRec [] _ _ _ = []

-- Resolve x component of a 3-dimensional Vector
v3x :: Vector3 a -> a
v3x (Vector3 x _ _) = x
-- Resolve y component of a 3-dimensional Vector
v3y :: Vector3 a -> a
v3y (Vector3 _ y _) = y
-- Resolve z component of a 3-dimensional Vector
v3z :: Vector3 a -> a
v3z (Vector3 _ _ z) = z

-- Cross product of two vectors
v3cross :: (Num a) => Vector3 a -> Vector3 a -> Vector3 a
v3cross (Vector3 a1 a2 a3) (Vector3 b1 b2 b3) = Vector3 c1 c2 c3
  where c1 = a2 * b3 - a3 * b2
        c2 = a3 * b1 - a1 * b3
        c3 = a1 * b2 - a2 * b1

v3add :: (Num a) => Vector3 a -> Vector3 a -> Vector3 a
v3add a b = Vector3 z y z
  where x = v3x a + v3x b
        y = v3y a + v3y b
        z = v3z a + v3z b
v3sub :: (Num a) => Vector3 a -> Vector3 a -> Vector3 a
v3sub a b = Vector3 z y z
  where x = v3x a - v3x b
        y = v3y a - v3y b
        z = v3z a - v3z b
v3sum :: (Num a) => [Vector3 a] -> Vector3 a
v3sum vlist = foldl (\accum v -> v3add v accum) (Vector3 0 0 0) vlist

v3div :: (Fractional a, Num a) => Vector3 a -> Vector3 a -> Vector3 a
v3div a b = Vector3 x y z
  where x = v3x a / v3x b
        y = v3y a / v3y b
        z = v3z a / v3z b

v3mul :: (Num a) => Vector3 a -> Vector3 a -> Vector3 a
v3mul a b = Vector3 x y z
  where x = v3x a * v3x b
        y = v3y a * v3y b
        z = v3z a * v3z b

-- Useful for mapping over a list containing tuples of (vertex, vertexNormal): 
vertexWithNormal :: (VertexComponent v, NormalComponent n) => (Vertex3 v, Normal3 n) -> IO ()
vertexWithNormal (v,n) = do normal n
                            vertex v 

-- Expects a sample matrix (SignalList) and returns a matrix of same dimensions 
-- containing tuples of (vertex,normalVector). 
verticesAndNormalsFromSignalList :: SignalList -> GLfloat -> GLfloat -> [[ (Vertex3 GLfloat, Normal3 GLfloat) ]]
verticesAndNormalsFromSignalList signalList vscale signalLineDist = [ [ (Vertex3 1.0 1.0 (0.0::GLfloat), Normal3 1.0 0.0 (0.0 :: GLfloat)) ] ]
--  verticesAndNormalsFromSignalList signalList vscale signalLineDist = do $ 
--    let signal = signalList !! z
--    samples <- getElems $ DT.signalArray signal

-- ?? Random access op on IOArray? How? This would prevent having to transform signalList to [[SValue]]

getVertexAndNormal :: SignalList -> Int -> Int -> IO (Vertex3 SValue, Normal3 SValue)
getVertexAndNormal samples x z = do let numSignals = length (signalList samples)
                                    boundsSamples <- getBounds $ signalArray $ (signalList samples) !! 0 
                                    let numSamples = rangeSize boundsSamples
                                    let boundx = \a -> max 0 (min a numSamples) :: Int 
                                    let boundz = \a -> max 0 (min a numSignals) :: Int 
                                    sample  <- sampleAt z x
                                    sampleR <- sampleAt z (boundx (x+1))
                                    sampleT <- sampleAt (boundz (z+1)) x
                                    sampleL <- sampleAt z (boundx (x-1))
                                    sampleB <- sampleAt (boundz (z-1)) x
                                    let point   = Vector3 (fromIntegral x) sample (fromIntegral z)
                                    let pointR  = Vector3 (fromIntegral x) sampleR (fromIntegral z)
                                    let pointT  = Vector3 (fromIntegral x) sampleT (fromIntegral z)
                                    let pointL  = Vector3 (fromIntegral x) sampleL (fromIntegral z)
                                    let pointB  = Vector3 (fromIntegral x) sampleB (fromIntegral z)
                                    let v1 = v3sub pointR point 
                                    let v4 = v3sub pointT point 
                                    let v3 = v3sub pointL point 
                                    let v2 = v3sub pointB point 
                                    let n1 = v3cross v4 v1
                                    let n2 = v3cross v1 v2
                                    let n3 = v3cross v2 v3
                                    let n4 = v3cross v3 v4
                                    let n  = v3div (v3sum [ n1, n2, n3, n4 ]) (Vector3 4.0 4.0 (4.0 :: GLfloat)) -- no normalization here as GL.normalize is enabled
                                    let vertex = Vertex3 (v3x point) (v3y point) (v3z point)
                                    let normal = Normal3 (v3x n) (v3y n) (v3z n)
                                    return ( vertex, normal ) 
    where 
      sampleAt z' x' = readArray (signalArray ((signalList samples) !! z')) x'

