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
    nextPerspective, 
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

    getViewpointFromModelView, 

    vertexSectionIndices, 
    renderSurface, 

    vxIIR, 
    vxApplyIIR
) where

import Debug.Trace

import Data.IORef ( IORef )

import Data.Packed.Matrix as HM ( (><), Matrix )
import Numeric.LinearAlgebra.Algorithms as LA ( inv ) 
import Numeric.Container as NC ( vXm, fromList, toList )

import Data.Array ( Array )
import Data.Array.IArray ( (!), bounds, IArray(..), listArray, ixmap, amap )
import Data.Ix ( rangeSize )
import Data.List ( findIndex )
import qualified Drool.Types as DT ( SignalList(..), RenderPerspective(..) )
import Drool.ApplicationContext as AC ( ContextSettings(..) )
import Drool.Utils.SigGen as SigGen ( SValue, TValue, SignalGenerator(..) )
import Drool.Utils.FeatureExtraction as FE ( 
    SignalFeatures(..), SignalFeaturesList(..), 
    emptyFeatures, 
    FeatureTarget(..), featureTargetFromIndex )
import Drool.Utils.Conversions as Conv ( interleaveArrays, aZip ) 
import Graphics.Rendering.OpenGL ( 
    Vector3 (..), 
    Vertex3 (..), 
    Vertex4 (..), 
    Normal3 (..), 
    Color3(..),
    Color4(..),
    VertexComponent,
    renderPrimitive, 
    PrimitiveMode(..),
    vertex, normal, 
    color, 
    translate, 
    MatrixOrder(..), 
    getMatrixComponents, 
    GLmatrix, 
    ($=), 
    Face(..), 
    cullFace,
    GLfloat )
import qualified Graphics.Rendering.FTGL as FTGL


data RenderSettings = RenderSettings { signalGenerator :: SignalGenerator, 
                                       -- IORef to signal buffer: 
                                       signalBuf :: IORef DT.SignalList, 
                                       -- maps x index to x position: 
                                       xPosFun :: (Int -> Int -> RenderSettings -> GLfloat),
                                       -- maps signal index to z position: 
                                       zPosFun :: (Int -> Int -> RenderSettings -> GLfloat), 
                                       -- scales sample (vertex y position) according to x and z index: 
                                       scaleFun :: (SValue -> Int -> Int -> GLfloat), 
                                       -- Linear scaling of X axis
                                       xLinScale :: GLfloat, 
                                       -- Log scaling of X axis
                                       xLogScale :: GLfloat, 
                                       -- Linear scaling of Z axis
                                       zLinScale :: GLfloat, 
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
                                       numSamples :: Int, 
                                       
                                       tick :: Int, 
                                       
                                       fillFont :: IORef FTGL.Font, 
                                       gridFont :: IORef FTGL.Font } 

data DirectionX = LeftToRight | RightToLeft
  deriving ( Eq, Show )
data DirectionZ = FrontToBack | BackToFront
  deriving ( Eq, Show )

nextPerspective :: DT.RenderPerspective -> DT.RenderPerspective
nextPerspective cur = case cur of 
  DT.Isometric -> DT.Top
  DT.Top       -> DT.Front 
  DT.Front     -> DT.Side
  DT.Side      -> DT.Isometric

-- Expects a sample, t, number of samples in total, list of band range amplifiers, and returns amplified sample for t. 
applyBandRangeAmp :: SValue -> TValue -> Int -> [Float] -> SValue
-- {{{
applyBandRangeAmp s t nSamples amps = s * ampValue
  where numRanges      = length amps
        rangeWidth     = nSamples `div` numRanges                            -- ...[-------]...
        rangePos       = t `mod` rangeWidth                                  -- ...|....x..|...
        activeRangeIdx = max 0 $ ((fromIntegral t)-rangePos) `div` rangeWidth :: Int -- [.|x|.|.|.]
        -- Amp value of active range
        amp_1          = if length amps > activeRangeIdx then realToFrac $ amps !! activeRangeIdx else 1.0
        -- Amp value of next range, use range n as next range of range n
        nextRangeIdx   = max 0 $ min (activeRangeIdx+1) (numRanges-1) 
        amp_2          = if length amps > nextRangeIdx then realToFrac $ amps !! nextRangeIdx else 1.0
        ampValue       = amp_1 + ((amp_2-amp_1) * (fromIntegral (rangePos) / fromIntegral (rangeWidth-1)))
-- }}}

bandRangeAmpSamples :: [SValue] -> [Float] -> [SValue]
bandRangeAmpSamples samples amps = bandRangeAmpSamplesRec samples amps (length samples) 0

bandRangeAmpSamplesRec :: [SValue] -> [Float] -> Int -> TValue -> [SValue]
bandRangeAmpSamplesRec (x:xs) amps nSamples t = ampSample : (bandRangeAmpSamplesRec xs amps nSamples (t+1))
  where ampSample = (applyBandRangeAmp x t nSamples amps)
bandRangeAmpSamplesRec [] _ _ _ = []

scaleSamples :: [SValue] -> SValue -> [SValue]
scaleSamples samples a = map ( \s -> s * a ) samples

-- {{{

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

-- TODO: All these vector / matrix operations should be 
--       outsourced to BLAS. 

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

vx3add :: (Num a) => Vertex3 a -> Vertex3 a -> Vertex3 a
vx3add a b = Vertex3 x y z
  where x = vx3x a + vx3x b
        y = vx3y a + vx3y b
        z = vx3z a + vx3z b
{-
v3mul :: (Num a) => Vector3 a -> Vector3 a -> Vector3 a
v3mul a b = Vector3 x y z
  where x = v3x a * v3x b
        y = v3y a * v3y b
        z = v3z a * v3z b
-}

{-
n3normalize :: Normal3 GLfloat -> Normal3 GLfloat
n3normalize n = Normal3 x y z 
  where norm = sqrt $ ((n3x n)**2) + ((n3y n)**2) + ((n3z n)**2) :: GLfloat
        x = (n3x n) / norm 
        y = (n3y n) / norm
        z = (n3z n) / norm
-}
n3Invert :: Normal3 GLfloat -> Normal3 GLfloat
n3Invert n = Normal3 x y z
  where x = -(n3x n)
        y = -(n3y n)
        z = -(n3z n)

color3AddAlpha :: Color3 GLfloat -> GLfloat -> Color4 GLfloat
color3AddAlpha (Color3 r g b) a = Color4 r g b a

-- }}}

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

normalsFromVertices :: [[ Vertex3 GLfloat ]] -> [ Normal3 GLfloat ]
-- {{{
normalsFromVertices sigs = normalsFromVertices' sigs 0

normalsFromVertices' :: [[ Vertex3 GLfloat ]] -> Int -> [ Normal3 GLfloat ]
normalsFromVertices' sigs xIdx = case sigs of  
  (sigPrev:sig:sigNext:[]) -> if nSamples > 0 && xIdx < nSamples then resultNormal : normalsFromVertices' sigs (xIdx+1) else []
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
  _ -> []
-- }}}

verticesFromSamples :: [ SValue ] -> Int -> RenderSettings -> [ Vertex3 GLfloat ]
verticesFromSamples samples signalIdx renderSettings = zipWith ( \xIdx s -> let x = (xPosFun renderSettings) xIdx nSamples renderSettings
                                                                                y = (scaleFun renderSettings) s signalIdx xIdx
                                                                                z = (zPosFun renderSettings) signalIdx nSignals renderSettings in
                                                                            Vertex3 x y z ) [0..] samples
                                                                              where nSamples = length samples
                                                                                    nSignals = numSignals renderSettings

-- Expects a 2-dimensional list of vertices, considering the first dimension a z-Index, 
-- and a function mapping z-Indices to z-coordinates. 
-- Updates z-coordinate in every vertex to (zPosFunc zIndex). 
updateVerticesZCoord :: [[ Vertex3 GLfloat ]] -> (Int -> Int -> RenderSettings -> GLfloat) -> RenderSettings -> [[ Vertex3 GLfloat ]]
updateVerticesZCoord signalsVertices zPosFunc renderSettings = zipWith (\sigVertices zIdx -> setSignalZVal sigVertices zIdx) signalsVertices [0..]
  where setSignalZVal vertexList z = map (\v -> Vertex3 (vx3x v) (vx3y v) (zPosFunc (nSignals-1-z) nSignals renderSettings ) ) vertexList
        nSignals = length signalsVertices

getViewpointFromModelView :: GLmatrix GLfloat -> IO ( Vector3 GLfloat )
-- {{{
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
-- }}}

applyFeaturesToGrid :: FE.SignalFeatures -> FE.FeatureTarget -> AC.ContextSettings -> IO ()
-- {{{
applyFeaturesToGrid features target settings = do
  let loudness     = realToFrac $ FE.totalEnergy features
      basslevel    = realToFrac $ FE.bassEnergy features 
      lTarget      = FE.featureTargetFromIndex $ AC.featureSignalEnergyTargetIdx settings
      bTarget      = FE.featureTargetFromIndex $ AC.featureBassEnergyTargetIdx settings
      lCoeff       = if lTarget == target || target == FE.GlobalAndLocalTarget then (
                        realToFrac $ AC.featureSignalEnergyGridCoeff settings )
                     else 0.0
      bCoeff       = if bTarget == target || target == FE.GlobalAndLocalTarget then (
                        realToFrac $ AC.featureBassEnergyGridCoeff settings )
                     else 0.0 
      gBaseOpacity = (AC.gridOpacity settings) / 100.0 :: GLfloat
      gOpacity     = gBaseOpacity + (lCoeff * loudness) + (bCoeff * basslevel)
      gColor       = color3AddAlpha (AC.gridColor settings) gOpacity
  color $ gColor
-- }}}

applyFeaturesToSurface :: FE.SignalFeatures -> FE.FeatureTarget -> AC.ContextSettings -> IO ()
-- {{{
applyFeaturesToSurface features target settings = do
  let loudness     = realToFrac $ FE.totalEnergy features
      basslevel    = realToFrac $ FE.bassEnergy features 
      lTarget      = FE.featureTargetFromIndex $ AC.featureSignalEnergyTargetIdx settings
      bTarget      = FE.featureTargetFromIndex $ AC.featureBassEnergyTargetIdx settings
      lCoeff       = if lTarget == target || target == FE.GlobalAndLocalTarget then (
                        realToFrac $ AC.featureSignalEnergySurfaceCoeff settings )
                     else 0.0
      bCoeff       = if bTarget == target || target == FE.GlobalAndLocalTarget then (
                        realToFrac $ AC.featureBassEnergySurfaceCoeff settings )
                     else 0.0 
      sBaseOpacity = (AC.surfaceOpacity settings) / 100.0 :: GLfloat
      sOpacity     = sBaseOpacity + (lCoeff * loudness) + (bCoeff * basslevel)
      sColor       = color3AddAlpha (AC.surfaceColor settings) sOpacity
  color $ sColor
-- }}}

-- Render surface as single strips (for z : for x). 
-- Expects pairs of current and next signal data, 
-- with signal data being vertices, normals, and signal features. 
renderSignalSurfaceStrip :: PrimitiveMode -> 
                            DirectionX -> 
                            ( FE.SignalFeatures -> FE.FeatureTarget -> AC.ContextSettings -> IO () ) -> 
                            (Array Int (Vertex3 GLfloat), Array Int (Vertex3 GLfloat)) -> 
                            (Array Int (Normal3 GLfloat), Array Int (Normal3 GLfloat)) -> 
                            (FE.SignalFeatures, FE.SignalFeatures) -> 
                            AC.ContextSettings -> 
                            Int -> -- signal index
                            IO ()
-- {{{
renderSignalSurfaceStrip mode dirX fAppFun (vsArrCurr,vsArrNext) (nsArrCurr,nsArrNext) (fsCurr,fsNext) settings _ = (
  do -- Put vertices and normals in correct (interleaved) order for 
     -- rendendering: 
     let aMaxIdx array = snd $ bounds array
     let aMap array = ixmap (0,aMaxIdx array) (\i -> colIdcs !! i) array
                      where colIdcs = if dirX == LeftToRight then ( 
                                        [0..(aMaxIdx array)] ) 
                                      else ( 
                                        [ aMaxIdx array - x | x <- [0..(aMaxIdx array)] ] )
     let vsCurr = aMap vsArrCurr
     let vsNext = aMap vsArrNext
     let nsCurr = aMap nsArrCurr
     let nsNext = aMap nsArrNext
     -- Bottleneck here: If splitXEnd - splitXStart gets big, so do vsCurr and vsNext! 
     -- This is why we need Arrays for interleaving, not lists. Lists are really, really 
     -- slow in this case. 
     let sortedVertices = if dirX == LeftToRight then Conv.interleaveArrays vsCurr vsNext else Conv.interleaveArrays vsNext vsCurr
     let sortedNormals  = if dirX == LeftToRight then Conv.interleaveArrays nsCurr nsNext else Conv.interleaveArrays nsNext nsCurr

     fAppFun fsCurr FE.LocalTarget settings
     fAppFun fsNext FE.LocalTarget settings
     renderPrimitive mode ( 
       mapM_ vertexWithNormal (Conv.aZip sortedVertices sortedNormals) ) )
-- }}}

-- List of indices indicating the order in which vertices have to be drawn depending 
-- on Z-direction (BackToFront | FrontToBack). 
-- TODO: Check if there is a performance gain when creating final drawing indices here (by
--       depending on X-direction) instead of switching X-order in renderSignalSurfaceStrip. 
vertexSectionIndices :: DirectionX -> DirectionZ -> Int -> (Int,Int) -> (Int,Int) -> Array Int (Array Int Int)
-- {{{
vertexSectionIndices _ dirZ nSamples (zStartIdx,xStartIdx) (zEndIdx,xEndIdx) = listArray (0,((length list)-1)) $ map (\idcs -> listArray (0,((length idcs)-1)) idcs) list
  where absIndexBtF z x = (nSamples * z) + x 
        absIndexFtB z x = (nSamples * zEndIdx) + x - (nSamples * (z-zStartIdx))
        list = if dirZ == BackToFront then (
                  [ [ absIndexBtF zIdx xIdx | xIdx <- [xStartIdx..xEndIdx] ] | zIdx <- [zStartIdx..zEndIdx] ] )
               else (
                  [ [ absIndexFtB zIdx xIdx | xIdx <- [xStartIdx..xEndIdx] ] | zIdx <- [zStartIdx..zEndIdx] ] )
-- }}}

-- Render a section of the surface. A single section is one of four sub-rectangles of 
-- the surface (the surface is split once in X and Z axis). 
-- Using Arrays instead of lists here as we need efficient random access on elements. 
renderSurfaceSection :: DirectionX -> DirectionZ -> 
                        Array Int (Vertex3 GLfloat) -> 
                        Array Int (Normal3 GLfloat) -> 
                        Array Int (FE.SignalFeatures) -> 
                        (Int,Int) -> (Int,Int) -> 
                        Int -> 
                        AC.ContextSettings -> 
                        IO ()
-- {{{
renderSurfaceSection dirX dirZ vArray nArray fArray secStart@(zStartIdx,xStartIdx) secEnd@(zEndIdx,xEndIdx) nSamples settings = 
  if zEndIdx-zStartIdx > 0 && xEndIdx-xStartIdx > 0 then ( 
    do
      let nSecSignals = zEndIdx - zStartIdx -- Number of signals in section
      -- let nSecSamples = xEndIdx - xStartIdx -- Number of samples per signal in section

      -- returns array of arrays of vertex indices sorted in correct z-direction: 
      let vIndexMatrixZSorted = vertexSectionIndices dirX dirZ nSamples secStart secEnd
      -- returns lists of signal vertices in the order they will be rendered. 
      -- First dimension is signal vertices, second dimension is sample value. 
      
      let safeArrayAt xs idx fallback = if idx >= 0 && rangeSize (bounds xs) > idx then xs ! idx else fallback
      let nSignals = rangeSize $ bounds fArray
      let sigIdcs  = if dirZ == BackToFront then [zStartIdx..zEndIdx] else [ zEndIdx-x | x <- [0..zEndIdx] ]

      let vSecArray = amap ( \zArray -> amap ( \i -> vArray ! i ) zArray ) vIndexMatrixZSorted
      let nSecArray = amap ( \zArray -> amap ( \i -> nArray ! i ) zArray ) vIndexMatrixZSorted

      -- Render single surface strips of this section in correct Z- and X-order: 
      mapM_ ( \(sigIdx,secSigIdx) -> ( do let globalSigIdx = zEndIdx
                                              vsCurr = safeArrayAt vSecArray secSigIdx (listArray (0,-1) [])
                                              vsNext = safeArrayAt vSecArray (secSigIdx+1) vsCurr
                                              nsCurr = safeArrayAt nSecArray secSigIdx (listArray (0,-1) [])
                                              nsNext = safeArrayAt nSecArray (secSigIdx+1) nsCurr
                                              fCurr  = safeArrayAt fArray (nSignals-sigIdx-1) FE.emptyFeatures
                                              fNext  = safeArrayAt fArray (nSignals-sigIdx-1) fCurr
                                              vsTpl  = if dirZ == FrontToBack then (vsCurr,vsNext) else (vsNext,vsCurr)
                                              nsTpl  = if dirZ == FrontToBack then (nsCurr,nsNext) else (nsNext,nsCurr)
                                              fTpl   = if dirZ == FrontToBack then (fCurr,fNext) else (fNext,fCurr)
                                          renderSignalSurfaceStrip TriangleStrip dirX applyFeaturesToSurface vsTpl nsTpl fTpl settings globalSigIdx 
                                          translate $ Vector3 0 (0.01::GLfloat) 0
                                          renderSignalSurfaceStrip LineStrip dirX applyFeaturesToGrid vsTpl nsTpl fTpl settings globalSigIdx 
                                          translate $ Vector3 0 (-0.01::GLfloat) 0 ) ) (zip sigIdcs [0..nSecSignals])
  ) else return () 
-- }}}

-- Render surface as four sections, each with a different X- and Z-direction. 
renderSurface :: [[ Vertex3 GLfloat ]] -> 
                 [[ Normal3 GLfloat ]] -> 
                 [ FE.SignalFeatures ] -> 
                 Vector3 GLfloat -> 
                 Int -> 
                 AC.ContextSettings -> 
                 RenderSettings -> 
                 IO ()
-- {{{
renderSurface vBuf nBuf fBuf viewpoint nSamples settings renderSettings = do
  let nSignals   = length vBuf
      vBufMaxIdx = (nSignals * nSamples) - 1
      nBufMaxIdx = ((length nBuf) * nSamples) - 1
      fBufMaxIdx = (length fBuf) - 1
      vArray     = listArray (0,vBufMaxIdx) (concat vBuf)
      nArray     = listArray (0,nBufMaxIdx) (concat nBuf)
      fArray     = listArray (0,fBufMaxIdx) fBuf

  let zCoordFun = zPosFun renderSettings
      xCoordFun = xPosFun renderSettings
      splitZ    = case findIndex ( \z -> (zCoordFun (z+1) nSignals renderSettings) >= (v3z viewpoint) ) [0..(nSignals-1)] of 
                    Just idx -> nSignals-idx-1
                    Nothing  -> 0
      splitX    = case findIndex ( \x -> (xCoordFun x nSamples renderSettings) >= (v3x viewpoint) ) [0..(nSamples-1)] of 
                    Just idx -> idx
                    Nothing  -> if (xCoordFun 0 nSamples renderSettings) >= (v3x viewpoint) then 0 else (nSamples-1)

  let secTopLeftStartIdcs     = ( 0 :: Int, 0 :: Int)
      secTopLeftEndIdcs       = ( splitZ, splitX )
      secTopRightStartIdcs    = ( 0 :: Int, splitX )
      secTopRightEndIdcs      = ( splitZ, (nSamples-1) )
      secBottomLeftStartIdcs  = ( splitZ, 0 :: Int )
      secBottomLeftEndIdcs    = ( (nSignals-1), splitX )
      secBottomRightStartIdcs = ( splitZ, splitX )
      secBottomRightEndIdcs   = ( (nSignals-1), (nSamples-1) )

  let renderSec dX dZ sS sE   = renderSurfaceSection dX dZ vArray nArray fArray sS sE nSamples settings
  
  cullFace $= if v3y viewpoint > 0 then Just Back else Just Front
  
  renderSec LeftToRight BackToFront secTopLeftStartIdcs secTopLeftEndIdcs 
  renderSec RightToLeft BackToFront secTopRightStartIdcs secTopRightEndIdcs 
  renderSec LeftToRight FrontToBack secBottomLeftStartIdcs secBottomLeftEndIdcs 
  renderSec RightToLeft FrontToBack secBottomRightStartIdcs secBottomRightEndIdcs 
-- }}}

{-
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
-}

vxIIR :: (Fractional a) => Vertex3 a -> [Vertex3 a] -> [a] -> Vertex3 a
vxIIR sample samples coefs = foldl (\ac (i,(Vertex3 x y z)) -> vx3add ac (Vertex3 x ((coefs !! i) * y) z)) (Vertex3 0.0 0.0 0.0) (zip [0..num-1] (sample : (take num samples)))
  where num = length coefs

vxApplyIIR :: (Fractional a) => [Vertex3 a] -> [a] -> Int -> [Vertex3 a]
vxApplyIIR samples coefs num =  if num > 0 then vxApplyIIR (headSamples ++ [iirVertex] ++ tailSamples) coefs (num-1) else samples
  where iirVertex   = vxIIR curVertex tailSamples coefs
        curVertex   = samples !! (num-1)
        headSamples = take (num-1) samples
        tailSamples = drop num samples
  
{-
vxApplyIIR :: (Fractional a) => [[ Vertex3 a ]] -> [a] -> Int -> [[ Vertex3 a ]]
vxApplyIIR vBuf coefs num = updatedSignals ++ (drop num vBuf)
  where updatedSignals = map (\sigIdx,sigVertices -> map (\vIdx, v ->  ) (zip [0..maxVidx] sigVertices) ) (zip [0..num-1] (take num vBuf))
-}




