

module Drool.Utils.RenderHelpers (
    RenderSettings(..), 
    applyBandRangeAmp, 
    bandRangeAmpSamples, 

    vertexWithNormal, 
    normalsFromSamples, 
    normalsFromVertices, 
    verticesFromSamples, 
    updateVerticesZCoord, 
) where

import Debug.Trace

import Data.IORef ( IORef(..), newIORef ) 

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

data RenderSettings = RenderSettings { -- maps x index to x position: 
                                       xPosFun :: (Int -> GLfloat),
                                       -- maps signal index to z position: 
                                       zPosFun :: (Int -> GLfloat), 
                                       -- scales sample (vertex y position) according to x and z index: 
                                       scaleFun :: (SValue -> Int -> Int -> GLfloat), 
                                       -- Containing one normal vector for every sample vertex: 
                                       normalsBuf :: IORef [[ Normal3 GLfloat ]], 
                                       -- Containing all vertices, used for normals computation 
                                       -- and rendering: 
                                       vertexBuf :: IORef [[ Vertex3 GLfloat ]], 
                                       -- Current number of signals in signal buffer: 
                                       numSignals :: Int, 
                                       -- Number of samples in most recent signal: 
                                       numSamples :: Int } 


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

-- Resolve x component of a 3-dimensional Vertex
vx3x :: Vertex3 a -> a
vx3x (Vertex3 x _ _) = x
-- Resolve y component of a 3-dimensional Vertex
vx3y :: Vertex3 a -> a
vx3y (Vertex3 _ y _) = y
-- Resolve z component of a 3-dimensional Vertex
vx3z :: Vertex3 a -> a
vx3z (Vertex3 _ _ z) = z

vertexToVector :: (Num a) => Vertex3 a -> Vector3 a 
vertexToVector v = Vector3 (vx3x v) (vx3y v) (vx3z v)

-- Cross product of two vectors
v3cross :: (Num a) => Vector3 a -> Vector3 a -> Vector3 a
v3cross (Vector3 a1 a2 a3) (Vector3 b1 b2 b3) = Vector3 c1 c2 c3
  where c1 = a2 * b3 - a3 * b2
        c2 = a3 * b1 - a1 * b3
        c3 = a1 * b2 - a2 * b1

v3add :: (Num a) => Vector3 a -> Vector3 a -> Vector3 a
v3add a b = Vector3 x y z
  where x = v3x a + v3x b
        y = v3y a + v3y b
        z = v3z a + v3z b
v3sub :: (Num a) => Vector3 a -> Vector3 a -> Vector3 a
v3sub a b = Vector3 x y z
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
vertexWithNormal :: (Vertex3 GLfloat, Normal3 GLfloat) -> IO ()
vertexWithNormal (v,n) = do normal n
                            vertex v 
                            return ()

-- Expects three sample lists (first and last possibly empty) and returns list of normal vectors 
-- for every sample in the second sample list. 
normalsFromSamples' :: [[ SValue ]] -> Int -> GLfloat -> GLfloat -> Int -> [ Normal3 GLfloat ]
normalsFromSamples' sigs@(sigPrev:sig:sigNext:[]) numSamples xdist zdist xIdx = if xIdx < numSamples then normal : normalsFromSamples' sigs numSamples xdist zdist (xIdx+1) else []
  where boundx  = \a -> max 0 (min a (numSamples-1)) :: Int 
        sampleC = sig !! xIdx
        sampleR = sig !! (boundx (xIdx+1))
        sampleL = sig !! (boundx (xIdx-1))
        sampleT = if length sigPrev > 0 then sigPrev !! xIdx else sampleC
        sampleB = if length sigNext > 0 then sigNext !! xIdx else sampleC
        point   = Vector3 (xdist * fromIntegral xIdx) sampleC 0
        pointR  = Vector3 (xdist * fromIntegral (xIdx+1)) sampleR 0
        pointT  = Vector3 (xdist * fromIntegral xIdx) sampleT (2.0 * zdist)
        pointL  = Vector3 (xdist * fromIntegral (xIdx-1)) sampleL 0
        pointB  = Vector3 (xdist * fromIntegral xIdx) sampleB (-zdist)
        vR = v3sub pointR point   --   n4  T  n1
        vT = v3sub pointT point   --       |
        vL = v3sub pointL point   --    L--C--R
        vB = v3sub pointB point   --       |
        n1 = v3cross vT vR        --   n3  B  n2
        n2 = v3cross vR vB
        n3 = v3cross vB vL
        n4 = v3cross vL vT
        -- no normalization here as GL.normalize is enabled
        n  = v3div (v3sum [ n1, n2, n3, n4 ]) (Vector3 4.0 4.0 (4.0 :: GLfloat)) 
        normal = Normal3 (v3x n) (v3y n) (v3z n)

normalsFromSamples :: [[ SValue ]] -> Int -> GLfloat -> GLfloat -> [ Normal3 GLfloat ]
normalsFromSamples sigs numSamples xdist zdist = normalsFromSamples' sigs numSamples xdist zdist 0

normalsFromVertices' :: [[ Vertex3 GLfloat ]] -> Int -> Int -> [ Normal3 GLfloat ]
normalsFromVertices' sigs@(sigPrev:sig:sigNext:[]) numSamples xIdx = if xIdx < numSamples then normal : normalsFromVertices' sigs numSamples (xIdx+1) else []
  where boundx  = \a -> max 0 (min a (numSamples-1)) :: Int 
        vertexC = sig !! xIdx
        vertexR = sig !! (boundx (xIdx+1))
        vertexL = sig !! (boundx (xIdx-1))
        vertexT = if length sigPrev > xIdx then sigPrev !! xIdx else vertexC
        vertexB = if length sigNext > xIdx then sigNext !! xIdx else vertexC
        point   = vertexToVector vertexC
        pointR  = vertexToVector vertexR
        pointL  = vertexToVector vertexL
        pointT  = vertexToVector vertexT
        pointB  = vertexToVector vertexB
        vR = v3sub pointR point   --   n4  T  n1
        vT = v3sub pointT point   --       |
        vL = v3sub pointL point   --    L--C--R
        vB = v3sub pointB point   --       |
        n1 = v3cross vT vR        --   n3  B  n2
        n2 = v3cross vR vB
        n3 = v3cross vB vL
        n4 = v3cross vL vT
        -- no normalization here as GL.normalize is enabled
        n  = v3div (v3sum [ n1, n2, n3, n4 ]) (Vector3 4.0 4.0 (4.0 :: GLfloat)) 
        normal = Normal3 (v3x n) (v3y n) (v3z n)

normalsFromVertices :: [[ Vertex3 GLfloat ]] -> Int -> [ Normal3 GLfloat ]
normalsFromVertices sigs numSamples = normalsFromVertices' sigs numSamples 0

verticesFromSamples :: [ SValue ] -> Int -> RenderSettings -> [ Vertex3 GLfloat ]
verticesFromSamples samples signalIdx renderSettings = zipWith ( \xIdx s -> let x = (xPosFun renderSettings) xIdx
                                                                                y = (scaleFun renderSettings) s signalIdx xIdx
                                                                                z = (zPosFun renderSettings) signalIdx in
                                                                            Vertex3 x y z ) [0..] samples

-- Expects a 2-dimensional list of vertices, considering the first dimension a z-Index, 
-- and a function mapping z-Indices to z-coordinates. 
-- Updates z-coordinate in every vertex to (zPosFun zIndex). 
updateVerticesZCoord :: [[ Vertex3 GLfloat ]] -> (Int -> GLfloat) -> [[ Vertex3 GLfloat ]]
updateVerticesZCoord signalsVertices zPosFun = zipWith (\sigVertices zIdx -> setSignalZVal sigVertices zIdx) signalsVertices [0..]
  where setSignalZVal vertexList z = map (\v -> Vertex3 (vx3x v) (vx3y v) (zPosFun z) ) vertexList


