-----------------------------------------------------------------------------
--
-- Module      :  Drool.Utils.RenderHelpers
-- Copyright   :  Tobias Fuchs
-- License     :  AllRightsReserved
--
-- Maintainer  :  twh.fuchs@gmail.com
-- Stability   :  experimental
-- Portability :  POSIX
--
-- |
--
-----------------------------------------------------------------------------

{-# OPTIONS -O2 -Wall #-}

module Drool.Utils.RenderHelpers (
    RenderSettings(..), 
    DirectionX(..), 
    DirectionZ(..), 
    applyBandRangeAmp, 
    bandRangeAmpSamples, 
    scaleSamples, 

    vertexWithNormal, 
    normalsFromVertices, 
    verticesFromSamples, 
    updateVerticesZCoord, 
    drawNormal, 
    color3AddAlpha, 

    vertexToVector, 
    vx4ToVx3, 

    surfaceSplitIndices, 
    getViewpointFromModelView, 

    vertexSectionIndices, 
    renderSurface
) where

import Debug.Trace

import Data.IORef ( IORef(..) )

import Data.Packed.Matrix as HM ( (><), Matrix )
import Numeric.LinearAlgebra.Algorithms as LA ( Field, inv ) 
import Numeric.Container as NC ( vXm, fromList, toList )

import Data.Array ( Array, indices )
import Data.Array.IArray ( (!), bounds, IArray(..), listArray )
import Data.Ix ( rangeSize )
import Drool.Types ( SignalList(..) )
import Drool.ApplicationContext as AC ( ContextSettings(..) )
import Drool.Utils.SigGen ( SValue, TValue )
import Drool.Utils.FeatureExtraction as FE ( SignalFeatures(..), SignalFeaturesList(..), emptyFeatures )
import Drool.Utils.Conversions as Conv ( interleave ) 
import Graphics.Rendering.OpenGL ( 
    Vector3 (..), 
    Vertex3 (..), 
    Vertex4 (..), 
    Normal3 (..), 
    Color3(..),
    Color4(..),
    VertexComponent,
    NormalComponent, 
    renderPrimitive, 
    PrimitiveMode(..),
    vertex, normal, 
    color, 
    translate, 
    MatrixOrder(..), 
    getMatrixComponents, 
    GLmatrix, 
    ($=), 
    FrontFaceDirection(..), 
    frontFace, 
    Face(..), 
    cullFace,
    GLfloat )

data RenderSettings = RenderSettings { signalBuf :: IORef SignalList, 
                                       -- maps x index to x position: 
                                       xPosFun :: (Int -> GLfloat),
                                       -- maps signal index to z position: 
                                       zPosFun :: (Int -> GLfloat), 
                                       -- scales sample (vertex y position) according to x and z index: 
                                       scaleFun :: (SValue -> Int -> Int -> GLfloat), 
                                       -- Position of light 0
                                       lightPos0 :: Vertex4 GLfloat, 
                                       -- Position of light 1
                                       lightPos1 :: Vertex4 GLfloat, 
                                       -- Containing one normal vector for every sample vertex: 
                                       normalsBuf :: IORef [[ Normal3 GLfloat ]], 
                                       -- Containing all vertices, used for normals computation 
                                       -- and rendering: 
                                       vertexBuf :: IORef [[ Vertex3 GLfloat ]], 
                                       -- Containing one SignalFeatures component for every signal: 
                                       featuresBuf :: IORef (FE.SignalFeaturesList), 
                                       -- Current number of signals in signal buffer: 
                                       numSignals :: Int, 
                                       -- Number of samples in most recent signal: 
                                       numSamples :: Int } 

data DirectionX = LeftToRight | RightToLeft
  deriving ( Eq, Show )
data DirectionZ = FrontToBack | BackToFront
  deriving ( Eq, Show )


-- Expects a sample, t, number of samples in total, list of band range amplifiers, and returns amplified sample for t. 
applyBandRangeAmp :: SValue -> TValue -> Int -> [Float] -> SValue
applyBandRangeAmp s t nSamples amps = s * ampValue
  where numRanges      = length amps
        rangeWidth     = nSamples `div` numRanges                          -- ...[-------]...
        rangePos       = t `mod` rangeWidth                                  -- ...|....x..|...
        activeRangeIdx = ((fromIntegral t)-rangePos) `div` rangeWidth :: Int -- [.|x|.|.|.]
        amp_1          = realToFrac $ amps !! activeRangeIdx                         -- Amp value of active range
        amp_2          = realToFrac $ amps !! (min (activeRangeIdx+1) (numRanges-1)) -- Amp value of next range, use range n as next range of range n
        ampValue       = amp_1 + ((amp_2-amp_1) * (fromIntegral (rangePos) / fromIntegral (rangeWidth-1)))
        
bandRangeAmpSamples :: [SValue] -> [Float] -> [SValue]
bandRangeAmpSamples samples amps = bandRangeAmpSamplesRec samples amps (length samples) 0

bandRangeAmpSamplesRec :: [SValue] -> [Float] -> Int -> TValue -> [SValue]
bandRangeAmpSamplesRec (x:xs) amps nSamples t = ampSample : (bandRangeAmpSamplesRec xs amps nSamples (t+1))
  where ampSample = (applyBandRangeAmp x t nSamples amps)
bandRangeAmpSamplesRec [] _ _ _ = []

scaleSamples :: [SValue] -> SValue -> [SValue]
scaleSamples samples a = map ( \s -> s * a ) samples

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

-- Resolve x component of a 3-dimensional Normal
n3x :: Normal3 a -> a
n3x (Normal3 x _ _) = x
-- Resolve y component of a 3-dimensional Normal
n3y :: Normal3 a -> a
n3y (Normal3 _ y _) = y
-- Resolve z component of a 3-dimensional Normal
n3z :: Normal3 a -> a
n3z (Normal3 _ _ z) = z

vx4ToVx3 :: Vertex4 a -> Vertex3 a
vx4ToVx3 (Vertex4 x y z _) = Vertex3 x y z

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

n3normalize :: Normal3 GLfloat -> Normal3 GLfloat
n3normalize n = Normal3 x y z 
  where norm = sqrt $ ((n3x n)**2) + ((n3y n)**2) + ((n3z n)**2) :: GLfloat
        x = (n3x n) / norm 
        y = (n3y n) / norm
        z = (n3z n) / norm

n3Invert :: Normal3 GLfloat -> Normal3 GLfloat
n3Invert n = Normal3 x y z
  where x = -(n3x n)
        y = -(n3y n)
        z = -(n3z n)

color3AddAlpha :: Color3 GLfloat -> GLfloat -> Color4 GLfloat
color3AddAlpha (Color3 r g b) a = Color4 r g b a

-- Useful for mapping over a list containing tuples of (vertex, vertexNormal): 
vertexWithNormal :: (Vertex3 GLfloat, Normal3 GLfloat) -> IO ()
vertexWithNormal (v,n) = do normal n
                            vertex v 
                            return ()

drawNormal :: (Fractional a, VertexComponent a) => ( Vertex3 a, Normal3 a ) -> IO ()
drawNormal (v,n) = do let from = v
                      let nx   = (n3x n) * 100.0
                      let ny   = (n3y n) * 100.0
                      let nz   = (n3z n) * 100.0
                      let to   = Vertex3 (nx + vx3x v) (ny + vx3y v) (nz + vx3z v)
                      color $ (Color4 1.0 1.0 1.0 1.0 :: Color4 GLfloat)
                      renderPrimitive Lines ( do vertex from
                                                 vertex to )

normalsFromVertices' :: [[ Vertex3 GLfloat ]] -> Int -> [ Normal3 GLfloat ]
normalsFromVertices' sigs@(sigPrev:sig:sigNext:[]) xIdx = if xIdx < nSamples then resultNormal : normalsFromVertices' sigs (xIdx+1) else []
  where nSamples = length sig
        boundx   = \a -> max 0 (min a (nSamples-1)) :: Int 
        vertexC  = sig !! xIdx
        vertexR  = sig !! (boundx (xIdx+1))
        vertexL  = sig !! (boundx (xIdx-1))
        vertexT  = if length sigPrev > xIdx then sigPrev !! xIdx else vertexC
        vertexB  = if length sigNext > xIdx then sigNext !! xIdx else vertexC
        point    = vertexToVector vertexC
        pointR   = vertexToVector vertexR
        pointL   = vertexToVector vertexL
        pointT   = vertexToVector vertexT
        pointB   = vertexToVector vertexB
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
        resultNormal = n3Invert $ Normal3 (v3x n) (v3y n) (v3z n)

normalsFromVertices :: [[ Vertex3 GLfloat ]] -> [ Normal3 GLfloat ]
normalsFromVertices sigs = normalsFromVertices' sigs 0

verticesFromSamples :: [ SValue ] -> Int -> RenderSettings -> [ Vertex3 GLfloat ]
verticesFromSamples samples signalIdx renderSettings = zipWith ( \xIdx s -> let x = (xPosFun renderSettings) xIdx
                                                                                y = (scaleFun renderSettings) s signalIdx xIdx
                                                                                z = (zPosFun renderSettings) signalIdx in
                                                                            Vertex3 x y z ) [0..] samples

-- Expects a 2-dimensional list of vertices, considering the first dimension a z-Index, 
-- and a function mapping z-Indices to z-coordinates. 
-- Updates z-coordinate in every vertex to (zPosFunc zIndex). 
updateVerticesZCoord :: [[ Vertex3 GLfloat ]] -> (Int -> GLfloat) -> [[ Vertex3 GLfloat ]]
updateVerticesZCoord signalsVertices zPosFunc = zipWith (\sigVertices zIdx -> setSignalZVal sigVertices zIdx) signalsVertices [0..]
  where setSignalZVal vertexList z = map (\v -> Vertex3 (vx3x v) (vx3y v) (zPosFunc (nSignals-1-z)) ) vertexList
        nSignals = length signalsVertices

getViewpointFromModelView :: GLmatrix GLfloat -> IO ( Vector3 GLfloat )
getViewpointFromModelView mvMatrix = do 
  mvMatrixRows <- getMatrixComponents ColumnMajor mvMatrix
  -- OpenGL float matrix to GSL double matrix: 
  let mvMatrixGSL = (4 >< 4) $ map (\e -> realToFrac e :: Double) mvMatrixRows
  -- Invert GSL matrix: 
  let mvMatrixGSLInv = LA.inv ( mvMatrixGSL :: HM.Matrix Double )
  -- 4th column vector is position of view point. First 3 columns are 
  -- transformation elements. 
  let viewPointProjection = NC.fromList [ 0.0, 0.0, 0.0, 1.0 ]
  let viewPointModelView = NC.toList $ NC.vXm viewPointProjection mvMatrixGSLInv
  let viewPoint = Vector3 (realToFrac $ viewPointModelView !! 0) 
                          (realToFrac $ viewPointModelView !! 1) 
                          (realToFrac $ viewPointModelView !! 2) :: Vector3 GLfloat
  return viewPoint

-- Resolves index of row where drawing order has to be switched (e.g. from 
-- left-to-right to right-to-left) depending on given position of viewpoint. 
-- Returns length of vertex array if drawing order does not have to be switched. 
surfaceSplitIndices :: Array Int (Vertex3 GLfloat) -> Vector3 GLfloat -> Int -> (Int,Int)
surfaceSplitIndices vArray viewpoint nSamples = (indexZ, indexX)
  where rowIdcs  = [ i | i <- [ 0..(nSignals-1)] ]
        colIdcs  = [ i | i <- [ 0..(nSamples-1)] ]
        nSignals = vBufLen `div` nSamples
        vBufLen  = rangeSize $ bounds vArray
        indexZ   = surfaceSplitIndexZ vArray viewpoint nSamples rowIdcs
        indexX   = surfaceSplitIndexX vArray viewpoint nSamples indexZ colIdcs

surfaceSplitIndexZ :: Array Int (Vertex3 GLfloat) -> Vector3 GLfloat -> Int -> [Int] -> Int
surfaceSplitIndexZ vArray viewpoint nSamples rowIndices@(ri:_:_) = 
  if vx3z (vArray ! i) <= v3z viewpoint then ri else recurse
  where recurse = surfaceSplitIndexZ vArray viewpoint nSamples (tail rowIndices)
        i = ri * nSamples
surfaceSplitIndexZ _ _ _ (ri:[]) = ri 
surfaceSplitIndexZ _ _ _ [] = 0

surfaceSplitIndexX :: Array Int (Vertex3 GLfloat) -> Vector3 GLfloat -> Int -> Int -> [Int] -> Int
surfaceSplitIndexX vArray viewpoint nSamples switchIndexZ colIndices@(ci:_:_) = 
  if vx3x (vArray ! i) >= v3x viewpoint then ci else recurse
  where recurse = surfaceSplitIndexX vArray viewpoint nSamples switchIndexZ (tail colIndices)
        i = (switchIndexZ * nSamples + ci)
surfaceSplitIndexX _ _ _ _ (ci:[]) = ci 
surfaceSplitIndexX _ _ _ _ [] = 0

{-
-- {{{ 
drawIndices' :: Int -> Int -> Int -> [Int]
drawIndices' i cols rows = cur ++ if i < ((cols) * (rows)) then drawIndices' (i+1) cols rows else []
  where cur = preDegenerates ++ nonDegenerates ++ postDegenerates 
        nonDegenerates = [ row * cols + col, (row+1) * cols + col ]
        col   = i `mod` cols
        row   = (i-col) `div` cols
        preDegenerates  = if col == 0 then [row*cols] else []
        postDegenerates = if col == (cols-1) then [(row+1) * cols + (col)] else []

-- Resolves order in which to render vertices for a triangle strip surface 
-- using one surface strip per row. 
-- Resulting order is not depth-sorted. 
drawIndices :: Int -> Int -> [Int]
drawIndices cols rows = take (numDrawIndices cols rows) $ drawIndices' 0 cols rows

-- two extra vertices for every row, but only one extra vertex 
-- in first and last row. Vertices in non-first and non-last row
-- are visited twice: 
numDrawIndices :: Int -> Int -> Int
numDrawIndices cols rows = (rows-2) * (2*cols+2) + 2 * (cols+1)

-- Renders given vertex and normal buffer. To be used within a renderPrimitive
-- context. 
renderSurface :: Array Int (Vertex3 GLfloat) -> Array Int (Normal3 GLfloat) -> [Int] -> IO ()
renderSurface vArray nArray (i:idcs) = safeRecurse
  where safeRecurse = if i < (rangeSize nBounds) && i < (rangeSize vBounds) 
                      then do normal (nArray ! i)
                              vertex (vArray ! i)
                              renderSurface vArray nArray idcs
                              return ()
                      else return () 
        vBounds = bounds vArray
        nBounds = bounds nArray
renderSurface _ _ [] = return ()

renderSurfaceSimple :: [ Vertex3 GLfloat ] -> [ Normal3 GLfloat ] -> [Int] -> IO ()
renderSurfaceSimple vertexList normalsList idcs = do 
  let vArray = listArray (0, (length vertexList)-1) vertexList
  let nArray = listArray (0, (length normalsList)-1) normalsList
  renderPrimitive TriangleStrip ( renderSurface vArray nArray idcs ) 

renderSurfaceDA :: PrimitiveMode -> [[ Vertex3 GLfloat ]] -> [[ Normal3 GLfloat ]] -> Vector3 GLfloat -> IO ()
renderSurfaceDA mode vBuf nBuf viewpoint = do 

--  color $ (Color4 0.8 0.4 0.4 0.8 :: Color4 GLfloat)
--  translate $ Vector3 0 (0.1 :: GLfloat) 0
  renderPrimitive mode ( renderSurface vArray nArray vIndicesRight )

--  color $ (Color4 0.4 0.4 0.8 0.8 :: Color4 GLfloat)
--  translate $ Vector3 0 (-0.1 :: GLfloat) 0
  renderPrimitive mode ( renderSurface vArray nArray vIndicesLeft )

  where nBufList = concat nBuf
        vBufList = concat vBuf
        vArray = listArray (0, (length vBufList)-1) vBufList
        nArray = listArray (0, (length nBufList)-1) nBufList
        nSamples = if length vBuf > 0 then length $ vBuf !! 0 else 0
        nSignals = length vBuf
        vIndices = drawIndices nSamples nSignals
        (switchIndexZ, switchIndexX) = surfaceSplitIndices vArray viewpoint nSamples
        vIndicesRight = take numVIndicesSplit vIndices
        vIndicesLeft = reverse $ drop numVIndicesSplit vIndices
        numVIndicesSplit = numVIndices - (numDrawIndices nSamples switchIndexZ)
        numVIndices = numDrawIndices nSamples nSignals
-- }}}
-}

applyFeaturesToGrid :: FE.SignalFeatures -> AC.ContextSettings -> IO ()
applyFeaturesToGrid features settings = do
  let loudCoeff       = realToFrac $ FE.totalEnergy features
      bassCoeff       = realToFrac $ FE.bassEnergy features 
      gBaseOpacity    = (AC.gridOpacity settings) / 100.0 :: GLfloat
      gOpacity        = gBaseOpacity * ( 0.5 * loudCoeff + 0.5 * bassCoeff )
      gColor          = color3AddAlpha (AC.gridColor settings) gOpacity
  color $ gColor

applyFeaturesToSurface :: FE.SignalFeatures -> AC.ContextSettings -> IO ()
applyFeaturesToSurface features settings = do
  let loudCoeff    = realToFrac $ FE.totalEnergy features
      bassCoeff    = realToFrac $ FE.bassEnergy features 
      sBaseOpacity = (AC.surfaceOpacity settings) / 100.0 :: GLfloat
      sOpacity     = sBaseOpacity * ( 0.2 * loudCoeff + 0.8 * bassCoeff )
      sColor       = color3AddAlpha (AC.surfaceColor settings) sOpacity
  color $ sColor

-- Render surface as single strips (for z : for x). 
-- Expects pairs of current and next signal data, 
-- with signal data being vertices, normals, and signal features. 
renderSignalSurfaceStrip :: PrimitiveMode -> 
                            ( FE.SignalFeatures -> AC.ContextSettings -> IO () ) -> 
                            DirectionX -> 
                            ([Vertex3 GLfloat], [Vertex3 GLfloat]) -> 
                            ([Normal3 GLfloat], [Normal3 GLfloat]) -> 
                            (FE.SignalFeatures, FE.SignalFeatures) -> 
                            AC.ContextSettings -> 
                            Int -> 
                            IO ()
renderSignalSurfaceStrip mode fAppFun dir (vsCurr,vsNext) (nsCurr,nsNext) (fsCurr,_) settings idx = (
  do -- Put vertices and normals in correct (interleaved) order for 
     -- rendendering: 
     let vertices = Conv.interleave vsCurr vsNext  -- [ s_0_0, s_1_0,  s_0_1, s_1_1, ... ]
     let normals  = Conv.interleave nsCurr nsNext  -- [ n_0_0, n_1_0,  n_0_1, n_1_1, ... ]

     -- TODO: Use Arrays here! 
     let sortedVertices = if dir == LeftToRight then vertices else reverse vertices
     let sortedNormals  = if dir == LeftToRight then normals  else reverse normals

     fAppFun fsCurr settings
     renderPrimitive mode ( 
       mapM_ vertexWithNormal (zip sortedVertices sortedNormals) ) )

vertexSectionIndices :: Int -> (Int,Int) -> (Int,Int) -> [[ Int ]]
vertexSectionIndices nSamples (zStartIdx,xStartIdx) (zEndIdx,xEndIdx) = 
  [ [ absIndex zIdx xIdx | xIdx <- [xStartIdx..xEndIdx] ] | zIdx <- [zStartIdx..zEndIdx] ]
  where absIndex z x = (nSamples * z) + x 

-- Using Arrays instead of lists here as we need efficient random access on element. 
renderSurfaceSection :: DirectionX -> DirectionZ -> 
                        Array Int (Vertex3 GLfloat) -> 
                        Array Int (Normal3 GLfloat) -> 
                        Array Int (FE.SignalFeatures) -> 
                        (Int,Int) -> (Int,Int) -> 
                        Int -> 
                        AC.ContextSettings -> 
                        IO ()
renderSurfaceSection dirX dirZ vArray nArray fArray secStart@(zStartIdx,xStartIdx) secEnd@(zEndIdx,xEndIdx) nSamples settings = do
    let nSecSignals = zEndIdx - zStartIdx -- Number of signals in section
    -- let nSecSamples = xEndIdx - xStartIdx -- Number of samples per signal in section

    -- returns lists of vertex indices sorted from left to right, back to front. 
    -- TODO: Use vertexSectionIndices dirZ nSamples secStart secEnd 
    --       instead of reversing the result!
    let vIndexMatrix = vertexSectionIndices nSamples secStart secEnd
    let vIndexMatrixZSorted = if dirZ == FrontToBack then reverse vIndexMatrix else vIndexMatrix
    -- returns lists of signal vertices in the order they will be rendered. 
    -- First dimension is signal vertices, second dimension is sample value. 
    -- Second dimension is sorted left to right. 
    -- We do not sort according to X-direction here, this is taken care of 
    -- in renderSignalSurfaceStrip. 
    --
    -- TODO: Use Arrays here! 
    let secVertices = map (\idcs -> map (\i -> vArray ! i) idcs) vIndexMatrixZSorted
    let secNormals  = map (\idcs -> map (\i -> nArray ! i) idcs) vIndexMatrixZSorted
    let secFeaturesBtF = map (\sIdx -> fArray ! sIdx) [zStartIdx..zEndIdx]
    let secFeaturesFtB = map (\sIdx -> fArray ! sIdx) [ zEndIdx-x | x <- [zStartIdx..zEndIdx] ]
    let secFeaturesZSorted = if dirZ == FrontToBack then secFeaturesFtB else secFeaturesBtF
    let safeAt xs idx fallback = if length xs > idx then xs !! idx else fallback
    -- Render single surface strips of this section in correct Z-order: 
    mapM_ ( \secSigIdx -> ( do let globalSigIdx = zEndIdx
                                   vsCurr = safeAt secVertices secSigIdx []
                                   vsNext = safeAt secVertices (secSigIdx+1) vsCurr
                                   nsCurr = safeAt secNormals secSigIdx []
                                   nsNext = safeAt secNormals (secSigIdx+1) nsCurr
                                   fCurr  = safeAt secFeaturesZSorted secSigIdx FE.emptyFeatures
                                   fNext  = safeAt secFeaturesZSorted (secSigIdx+1) fCurr
                               renderSignalSurfaceStrip TriangleStrip applyFeaturesToSurface dirX (vsCurr,vsNext) (nsCurr,nsNext) (fCurr,fNext) settings globalSigIdx 
                               translate $ Vector3 0 (0.01::GLfloat) 0
                               renderSignalSurfaceStrip LineStrip applyFeaturesToGrid dirX (vsCurr,vsNext) (nsCurr,nsNext) (fCurr,fNext) settings globalSigIdx 
                               translate $ Vector3 0 (-0.01::GLfloat) 0 ) ) [0..nSecSignals] 

-- Render surface as four sections, each with a different X- and Z-direction. 
renderSurface :: [[ Vertex3 GLfloat ]] -> 
                 [[ Normal3 GLfloat ]] -> 
                 [ FE.SignalFeatures ] -> 
                 Vector3 GLfloat -> 
                 Int -> 
                 AC.ContextSettings -> 
                 IO ()
renderSurface vBuf nBuf fBuf viewpoint nSamples settings = do
  let vBuf'    = concat vBuf
      nBuf'    = concat nBuf
      vBufSize = (length vBuf') - 1
      nBufSize = (length nBuf') - 1
      fBufSize = (length fBuf) - 1
      vArray   = listArray (0,vBufSize) vBuf'
      nArray   = listArray (0,nBufSize) nBuf'
      fArray   = listArray (0,fBufSize) fBuf

  let (splitZ,splitX) = surfaceSplitIndices vArray viewpoint nSamples 
      nSignals = (rangeSize $ bounds vArray) `div` nSamples
  
  let secTopLeftStartIdcs     = ( 0 :: Int, 0 :: Int)
      secTopLeftEndIdcs       = ( splitZ, splitX )
      secTopRightStartIdcs    = ( 0 :: Int, splitX )
      secTopRightEndIdcs      = ( splitZ, (nSamples-1) )
      secBottomLeftStartIdcs  = ( splitZ, 0 :: Int )
      secBottomLeftEndIdcs    = ( (nSignals-1), splitX )
      secBottomRightStartIdcs = ( splitZ, splitX )
      secBottomRightEndIdcs   = ( (nSignals-1), (nSamples-1) )

  let renderSec dX dZ sS sE   = renderSurfaceSection dX dZ vArray nArray fArray sS sE nSamples settings
  
  cullFace $= Just Front
  renderSec LeftToRight BackToFront secTopLeftStartIdcs secTopLeftEndIdcs 
  renderSec RightToLeft BackToFront secTopRightStartIdcs secTopRightEndIdcs 
  cullFace $= Just Back
  renderSec LeftToRight FrontToBack secBottomLeftStartIdcs secBottomLeftEndIdcs 
  renderSec RightToLeft FrontToBack secBottomRightStartIdcs secBottomRightEndIdcs 

