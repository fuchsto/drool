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
    applyGlobalRotation, 
    applyPerspective, 
    nextPerspective, 
    useLight, 
    applyBandRangeAmp, 
    bandRangeAmpSamples, 
    scaleSamples, 

    vertexWithNormal, 
    vertexWithNormalAndColor, 
    normalsFromVertices, 
    drawNormal, 
    color3AddAlpha, 
    color4MulAlpha, 
    color3MulValue, 
    color4MulValue, 

    vertexToVector, 
    vx4ToVx3, 

    v3x, 
    v3y, 
    v3z, 
    vx3x, 
    vx3y, 
    vx3z, 
    vx4x, 
    vx4y, 
    vx4z, 
    vx4w, 

    getViewpointFromModelView, 
    featuresToIntensity
) where

import Debug.Trace

-- Imports
-- {{{ 
import Data.IORef ( IORef )

import Data.Packed.Matrix as HM ( (><), Matrix )
import Numeric.LinearAlgebra.Algorithms as LA ( inv ) 
import Numeric.Container as NC ( vXm, fromList, toList )

import qualified Drool.Types as DT ( SignalList(..), RenderPerspective(..), RotationVector(..) )
import Drool.Utils.SigGen as SigGen ( SValue, TValue, SignalGenerator(..) )
import Drool.Utils.FeatureExtraction as FE ( 
    SignalFeatures(..), SignalFeaturesList(..), 
    emptyFeatures, 
    FeatureTarget(..), featureTargetFromIndex )
import Drool.Utils.Conversions as Conv ( interleaveArrays, aZip ) 
import Drool.ApplicationContext as AC ( ContextSettings(..), LightConfig(..) )
import Graphics.Rendering.OpenGL ( 
    Vector3 (..), 
    Vertex3 (..), 
    Vertex4 (..), 
    Normal3 (..), 
    Color3(..),
    Color4(..),
    Light(..), 
    light, 
    VertexComponent,
    renderPrimitive, 
    PrimitiveMode(..),
    vertex, normal, 
    color, 
    materialEmission, 
    materialAmbient, 
    materialDiffuse, 
    materialSpecular,
    materialShininess,
    translate, 
    MatrixOrder(..), 
    getMatrixComponents, 
    GLmatrix, 
    ($=), 
    Face(..), 
    Capability(..),
    polygonOffsetFill, 
    polygonOffsetLine, 
    ambient, 
    diffuse, 
    specular, 
    rotate, 
    translate, 
    GLfloat )
import qualified Graphics.Rendering.FTGL as FTGL
import qualified Control.Concurrent.MVar as MV ( MVar )
import qualified Control.Concurrent.Chan as CC
-- }}}

-- Settings rendering independent from visual used: 
data RenderSettings = RenderSettings { signalGenerator :: SignalGenerator, 
                                       samplingSem :: MV.MVar Int, 
                                       renderingSem :: MV.MVar Int, 
                                       numNewSignalsChan :: CC.Chan Int, 
                                       -- IORef to signal buffer: 
                                       signalBuf :: IORef DT.SignalList, 
                                       -- Position of light 0
                                       lightPos0 :: Vertex4 GLfloat, 
                                       -- Position of light 1
                                       lightPos1 :: Vertex4 GLfloat, 
                                       -- Containing one SignalFeatures component for every signal: 
                                       featuresBuf :: IORef (FE.SignalFeaturesList), 
                                       -- Current number of signals in signal buffer: 
                                       numSignals :: Int, 
                                       -- Number of samples in most recent signal: 
                                       numSamples :: Int, 
                                       -- Number of new signals since last render pass: 
                                       numNewSignals :: Int, 
                                       -- Whether to reverse the signal buffer: 
                                       reverseBuffer :: Bool, 
                                       -- Tick of rendering pass: 
                                       tick :: Int }

applyPerspective :: DT.RenderPerspective -> IO ()
applyPerspective p = do
  case p of
    DT.Isometric -> do
      rotate (45::GLfloat) $ Vector3 1.0 0.0 0.0
      rotate (45::GLfloat) $ Vector3 0.0 1.0 0.0
    DT.Top -> do
      rotate (90::GLfloat) $ Vector3 1.0 0.0 0.0
    DT.Front -> do
      rotate (20.0::GLfloat) $ Vector3 1.0 0.0 0.0
    DT.Side -> do
      rotate (20.0::GLfloat) $ Vector3 1.0 0.0 0.0
      rotate (-90::GLfloat) $ Vector3 0.0 1.0 0.0

applyGlobalRotation :: DT.RotationVector -> DT.RotationVector -> IO ()
applyGlobalRotation fixedRotation incRotation = do
  rotate (DT.rotX fixedRotation) $ Vector3 1.0 0.0 0.0
  rotate (DT.rotX incRotation)   $ Vector3 1.0 0.0 0.0
  rotate (DT.rotY fixedRotation) $ Vector3 0.0 1.0 0.0
  rotate (DT.rotY incRotation)   $ Vector3 0.0 1.0 0.0
  rotate (DT.rotZ fixedRotation) $ Vector3 0.0 0.0 1.0
  rotate (DT.rotZ incRotation)   $ Vector3 0.0 0.0 1.0

nextPerspective :: DT.RenderPerspective -> DT.RenderPerspective
nextPerspective cur = case cur of 
  DT.Isometric -> DT.Top
  DT.Top       -> DT.Front 
  DT.Front     -> DT.Side
  DT.Side      -> DT.Isometric

useLight :: AC.LightConfig -> IO ()
useLight lightConfig = do 
  let lightState = AC.lightState lightConfig
  let lightIdx   = fromIntegral $ AC.lightIndex lightConfig
  light (Light lightIdx) $= lightState
  let lighting state = 
        if state == Enabled then do
          let intensity = AC.lightIntensity lightConfig
          ambient  (Light lightIdx) $= color4MulValue (AC.lightAmbient  lightConfig) intensity
          diffuse  (Light lightIdx) $= color4MulValue (AC.lightDiffuse  lightConfig) intensity
          specular (Light lightIdx) $= color4MulValue (AC.lightSpecular lightConfig) intensity
        else
          return ()
  lighting lightState

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

scaleSamples :: (Fractional a) => [a] -> a -> [a]
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

-- Resolve x component of a 4-dimensional Vertex
vx4x :: Vertex4 a -> a
vx4x (Vertex4 x _ _ _) = x
-- Resolve y component of a 4-dimensional Vertex
vx4y :: Vertex4 a -> a
vx4y (Vertex4 _ y _ _) = y
-- Resolve z component of a 4-dimensional Vertex
vx4z :: Vertex4 a -> a
vx4z (Vertex4 _ _ z _) = z
-- Resolve w component of a 4-dimensional Vertex
vx4w :: Vertex4 a -> a
vx4w (Vertex4 _ _ _ w) = w

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

n3Invert :: Normal3 GLfloat -> Normal3 GLfloat
n3Invert n = Normal3 x y z
  where x = -(n3x n)
        y = -(n3y n)
        z = -(n3z n)

color3AddAlpha :: Color3 GLfloat -> GLfloat -> Color4 GLfloat
color3AddAlpha (Color3 r g b) a = Color4 r g b a

color4MulAlpha :: Color4 GLfloat -> GLfloat -> Color4 GLfloat
color4MulAlpha (Color4 r g b a) x = Color4 r g b (a*x)

color4MulValue :: Color4 GLfloat -> GLfloat -> Color4 GLfloat
color4MulValue (Color4 r g b a) x = Color4 (r*x) (g*x) (b*x) a

color3MulValue :: Color3 GLfloat -> GLfloat -> Color3 GLfloat
color3MulValue (Color3 r g b) x = Color3 (r*x) (g*x) (b*x) 

-- }}}

-- Useful for mapping over a list containing tuples of (vertex, vertexNormal): 
vertexWithNormal :: (Vertex3 GLfloat, Normal3 GLfloat) -> IO ()
vertexWithNormal (v,n) = do normal n
                            vertex v 
                            return ()

vertexWithNormalAndColor :: (Vertex3 GLfloat, Normal3 GLfloat) -> Color4 GLfloat -> IO ()
vertexWithNormalAndColor (v,n) c = do color c
                                      normal n
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

featuresToIntensity :: FE.SignalFeatures -> FE.FeatureTarget -> AC.ContextSettings -> (GLfloat,GLfloat)
featuresToIntensity features target cSettings = (lCoeff, bCoeff)
  where loudness     = realToFrac $ FE.totalEnergy features
        basslevel    = realToFrac $ FE.bassEnergy features 
        lTarget      = FE.featureTargetFromIndex $ AC.featureSignalEnergyTargetIdx cSettings
        bTarget      = FE.featureTargetFromIndex $ AC.featureBassEnergyTargetIdx cSettings
        lCoeff       = if lTarget == target || lTarget == FE.GlobalAndLocalTarget then (
                          realToFrac $ (AC.featureSignalEnergySurfaceCoeff cSettings) * loudness )
                       else 0.0
        bCoeff       = if bTarget == target || bTarget == FE.GlobalAndLocalTarget then (
                          realToFrac $ (AC.featureBassEnergySurfaceCoeff cSettings) * basslevel )
                       else 0.0 

