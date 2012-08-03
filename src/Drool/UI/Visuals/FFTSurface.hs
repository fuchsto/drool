-----------------------------------------------------------------------------
--
-- Module      :  Drool.UI.Visuals.FFTSurface
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

module Drool.UI.Visuals.FFTSurface (
    FFTSurface, -- hidden type constructor
    createFFTSurfaceVisual, 
    fftSurfaceNew, 
    fftSurfaceDimensions, 
    fftSurfaceRender, 
    fftSurfaceUpdate
) where

-- Imports
-- {{{
import Drool.UI.Visuals.Visual as Visual

import Drool.Utils.RenderHelpers as RH hiding ( reverseBuffer, numSamples )
import qualified Drool.Utils.RenderHelpers as RH ( numSamples )

import Data.IORef ( IORef, newIORef, readIORef, modifyIORef )

import Data.Array ( Array )
import Data.Array.IArray ( (!), bounds, IArray(..), listArray, ixmap, amap )
import Data.Array.IO ( getElems )
import Data.Ix ( rangeSize )
import Data.List ( findIndex )
import qualified Drool.Types as DT ( Signal(..), SignalList(..), RenderPerspective(..), getRecentSignal, getLastSignal, RotationVector(..) )
import qualified Drool.ApplicationContext as AC ( ContextSettings(..), MaterialConfig(..) )
import qualified Drool.Utils.SigGen as SigGen ( SValue, SignalGenerator(..) )
import Drool.Utils.FeatureExtraction as FE ( 
    SignalFeatures(..), SignalFeaturesList(..), 
    emptyFeatures, 
    FeatureTarget(..), featureTargetFromIndex )
import Drool.Utils.Conversions as Conv ( interleaveArrays, aZip, adjustBufferSize, adjustBufferSizeBack ) 
import Graphics.Rendering.OpenGL as GL ( 
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
    materialEmission, 
    materialAmbient, 
    materialDiffuse, 
    materialSpecular,
    materialShininess,
    translate, 
    MatrixOrder(..), 
    get, 
    preservingMatrix, 
    MatrixMode(..),
    getMatrixComponents, 
    GLmatrix, 
    ($=), 
    Face(..), 
    Capability(..),
    polygonOffsetFill, 
    polygonOffsetLine, 
    blendFunc, 
    BlendingFactor(..), 
    colorMaterial, 
    ColorMaterialParameter(..), 
    fog, 
    scale, 
    matrix, 
    polygonOffset, 
    polygonOffsetLine, 
    polygonOffsetFill, 
    PolygonMode(..), 
    GLfloat )
import qualified Graphics.Rendering.FTGL as FTGL
-- }}}

-- Settings for current visual, excluding those independent from 
-- visual used: 
data FFTSurface = FFTSurface { -- maps x index to x position: 
                               xPosFun :: (Int -> FFTSurface -> GLfloat),
                               -- maps signal index to z position: 
                               zPosFun :: (Int -> FFTSurface -> GLfloat), 
                               -- scales sample (vertex y position) according to x and z index: 
                               scaleFun :: (SigGen.SValue -> Int -> Int -> SigGen.SValue), 
                               -- Linear scaling of X axis
                               xLinScale :: GLfloat, 
                               -- Log scaling of X axis
                               xLogScale :: GLfloat, 
                               -- Linear scaling of Z axis
                               zLinScale :: GLfloat, 
                               -- Containing one normal vector for every sample vertex: 
                               normalsBuf :: IORef [[ Normal3 GLfloat ]], 
                               vertexBuf :: IORef [[ Vertex3 GLfloat ]], 

                               reverseBuffer :: Bool, 
                               
                               fillFont :: IORef FTGL.Font, 
                               gridFont :: IORef FTGL.Font, 
                               
                               -- Do not use IORefs. The visual must not change the 
                               -- global state of application and rendering context!
                               contextSettings :: AC.ContextSettings, 
                               renderSettings :: RH.RenderSettings }

instance VState FFTSurface where 
  vsRenderSettings = renderSettings

-- Hook Visual interface function definitions to concrete implementations: 
createFFTSurfaceVisual :: IORef AC.ContextSettings -> Visual FFTSurface
createFFTSurfaceVisual contextSettingsIORef = Visual { -- curried: RenderSettings -> IO (FFTSurface) 
                                                       newVisual  = fftSurfaceNew contextSettingsIORef, 
                                                       -- curried: FFTSurface
                                                       dimensions = fftSurfaceDimensions, 
                                                       -- curried: RenderSettings -> IORef FFTSurface -> Int -> IO (FFTSurface)
                                                       update     = fftSurfaceUpdate contextSettingsIORef, 
                                                       -- curried: FFTSurface 
                                                       render     = fftSurfaceRender } 

fftSurfaceNew :: IORef AC.ContextSettings -> RH.RenderSettings -> IO (FFTSurface)
-- {{{
fftSurfaceNew cSettingsIORef rSettings = do
  vertexBufIORef  <- newIORef []
  normalsBufIORef <- newIORef []

  cSettings <- readIORef cSettingsIORef 

  gridfont <- FTGL.createOutlineFont "ProggyClean.ttf"
  fillfont <- FTGL.createPolygonFont "ProggyClean.ttf"
  _ <- FTGL.setFontFaceSize gridfont 36 36 
  _ <- FTGL.setFontFaceSize fillfont 36 36 

  gridFontIORef <- newIORef gridfont
  fillFontIORef <- newIORef fillfont

  let xPosFunc x visual = (log (x'+1.0) + (x' / n' * xLog)) / (log n' + xLog) * xLin
                          where xLog = xLogScale visual
                                xLin = xLinScale visual
                                sGen = signalGenerator (renderSettings visual)
                                n' = fromIntegral $ RH.numSamples (renderSettings visual) -- SigGen.numSamples sGen 
                                x' = fromIntegral x

  -- Returns Infinity for numSignals = 0
  let zPosFunc z visual = fromIntegral z / n' * zLin
                          where zLin = zLinScale visual
                                n'   = fromIntegral nSig
                                nSig = RH.numSignals (renderSettings visual)

  let settings = FFTSurface { xPosFun         = xPosFunc, 
                              zPosFun         = zPosFunc, 
                              scaleFun        = (\s _ _ -> s), 
                              xLinScale       = AC.xLinScale cSettings, 
                              xLogScale       = AC.xLogScale cSettings, 
                              zLinScale       = AC.zLinScale cSettings,
                              vertexBuf       = vertexBufIORef, 
                              normalsBuf      = normalsBufIORef, 
                              reverseBuffer   = AC.reverseBuffer cSettings, 
                              fillFont        = fillFontIORef,
                              gridFont        = gridFontIORef, 
                              contextSettings = cSettings, 
                              renderSettings  = rSettings }
  return settings
-- }}}

fftSurfaceDimensions :: FFTSurface -> (GLfloat,GLfloat,GLfloat)
-- {{{
fftSurfaceDimensions visual = (width,height,depth)
  where width  = (xPosFun visual) (nSamples-1) visual
        depth  = (zPosFun visual) (nSignals-1) visual
        height = 2.0
        nSamples = RH.numSamples $ renderSettings visual
        nSignals = RH.numSignals $ renderSettings visual
-- }}}

fftSurfaceUpdate :: IORef AC.ContextSettings -> RH.RenderSettings -> IORef FFTSurface -> Int -> IO (FFTSurface)
-- {{{  
fftSurfaceUpdate cSettingsIORef rSettings visualIORef t = do
  cSettings <- readIORef cSettingsIORef

  -- Load current visual: 
  visualPrev <- readIORef visualIORef 
  let visual = visualPrev { xLinScale = AC.xLinScale cSettings, 
                            xLogScale = AC.xLogScale cSettings, 
                            zLinScale = AC.zLinScale cSettings }
  -- Update visual settings from application context: 
  modifyIORef visualIORef ( \_ -> visual )

  let signalBufIORef = RH.signalBuf rSettings
      hScale         = (AC.scaling cSettings) / (100.0::Float)
      surfOpacity    = (AC.surfaceOpacity cSettings) / (100.0::GLfloat)
      maxNumSignals  = AC.signalBufferSize cSettings
      sigGen         = RH.signalGenerator rSettings

  sigBuf <- readIORef signalBufIORef
  
  -- Load vertex buffer from rendering context
  let vertexBufIORef = vertexBuf visual
  -- Load normals buffer from rendering context
  let normalsBufIORef = normalsBuf visual

  let nNewSignals = RH.numNewSignals rSettings
  let newSignals  = take nNewSignals $ DT.signalList sigBuf
  let nSignals    = RH.numSignals rSettings
  let numNewSignalsRead = length newSignals

  let rangeAmps = AC.rangeAmps cSettings

  newSigVertices <- mapM ( \sig -> do sigSamples <- getElems $ DT.signalArray sig
                                      let sigSamplesT = RH.bandRangeAmpSamples (RH.scaleSamples sigSamples hScale) rangeAmps
                                      let sigVertices = verticesFromSamples sigSamplesT 0 visual
                                      return sigVertices ) (reverse newSignals)

  vBufCurr <- readIORef vertexBufIORef
  let vBufUpdated     = if numNewSignalsRead > 0 then newSigVertices ++ (Conv.adjustBufferSizeBack vBufCurr (nSignals-numNewSignalsRead)) else vBufCurr
  let vBufZAdjusted   = if numNewSignalsRead > 0 then updateVerticesZCoord vBufUpdated visual else vBufUpdated
  modifyIORef vertexBufIORef ( \_ -> vBufZAdjusted )

  nBufCurr <- readIORef normalsBufIORef
  let nBufUpdated = if numNewSignalsRead > 0 then updateNormalsBuffer nBufCurr (take (numNewSignalsRead+2) vBufZAdjusted) nSignals else nBufCurr
  modifyIORef normalsBufIORef ( \_ -> nBufUpdated )

  modifyIORef visualIORef (\vis -> vis { renderSettings  = rSettings, 
                                         contextSettings = cSettings })
  
  return visual
-- }}}

fftSurfaceRender :: FFTSurface -> IO ()
-- {{{    
fftSurfaceRender visual = do 
  let rSettings = renderSettings visual
  let cSettings = contextSettings visual

  vBuf <- readIORef $ vertexBuf visual
  nBuf <- readIORef $ normalsBuf visual
  fBuf <- readIORef $ RH.featuresBuf rSettings

  let nSamples     = RH.numSamples rSettings
      nSignals     = RH.numSignals rSettings
      xPosFunc     = xPosFun visual
      zPosFunc     = zPosFun visual
      surfaceWidth = xPosFunc (nSamples-1) visual
      surfaceDepth = zPosFunc (nSignals-1) visual
  
  -- Get inverse of model view matrix: 
  glModelViewMatrix <- GL.get (matrix (Just (Modelview 0))) :: IO (GLmatrix GLfloat)
  -- Resolve view point in model view coordinates: 
  viewpoint <- RH.getViewpointFromModelView glModelViewMatrix

  renderSurface vBuf nBuf (FE.signalFeaturesList fBuf) viewpoint nSamples cSettings visual

  -- Render marquee text, if any
  blendFunc $= ( One, One )
  -- colorMaterial $= Just (Front, AmbientAndDiffuse)
  let marqueeText = AC.marqueeText cSettings
  gridfont <- readIORef $ gridFont visual
  fillfont <- readIORef $ fillFont visual

  marqueeBBox <- FTGL.getFontBBox gridfont marqueeText 

  preservingMatrix $ do 
    fog $= Disabled
    let fontWidth = realToFrac $ (marqueeBBox !! 3) - (marqueeBBox !! 0)
    -- let fontHeight = realToFrac $ (marqueeBBox !! 4) - (marqueeBBox !! 1)
    let fontScaling = (surfaceWidth * 1.5) / fontWidth
    colorMaterial $= Just (Front, Ambient) 
    color $ AC.materialAmbient (AC.surfaceMaterial cSettings)
    GL.scale fontScaling fontScaling (0 :: GLfloat)
    translate $ Vector3 (-0.5 * fontWidth + 0.5 * surfaceWidth / fontScaling) (0.7 / fontScaling) (-4.5 :: GLfloat)
    FTGL.renderFont fillfont marqueeText FTGL.Front
    color $ Color4 0 0 0 (0.5::GLfloat)
    FTGL.renderFont gridfont marqueeText FTGL.Front
    fog $= Enabled
-- }}}

-- Helper functions: 

data DirectionX = LeftToRight | RightToLeft
  deriving ( Eq, Show )
data DirectionZ = FrontToBack | BackToFront
  deriving ( Eq, Show )

-- Expects current normals buffer, list of signals as vertices and maximum number of signals in buffers. 
-- For every signal in the second argument, a list of normal vectors is appended to the given 
-- normals buffer. 
-- Returns updated normals buffer. 
updateNormalsBuffer :: [[ Normal3 GLfloat ]] -> [[ Vertex3 GLfloat ]] -> Int -> [[ Normal3 GLfloat ]] 
-- {{{
updateNormalsBuffer nBuf vBuf@(_:_:_:_) nSignals = case vBuf of 
  (_:_:_:_) -> updateNormalsBuffer updatedNormalsBuf (tail vBuf) nSignals
               where 
               -- Drop first and last element of nBuf. 
               -- Last element has incomplete normals and will be replaced by element 
               -- with correct normals. This happens in any case. 
               -- Drop first element if max buffer size is exceeded. 
                 adjNormalsBuf = take (nSignals-2) (Conv.adjustBufferSize nBuf (nSignals-1))
                 vBufLen = length vBuf
               
               -- Take 3 most recent signals from vertex buffer. 
               -- Last element in signal buffer is most recent signal: 
                 sigCurrIdx  = 0
                 sigPrev1Idx = if length vBuf > 1 then 1 else sigCurrIdx
                 sigPrev2Idx = if length vBuf > 2 then 2 else sigPrev1Idx
                 
                 sigCurr  = if vBufLen > 0 then (vBuf !! sigCurrIdx)  else [Vertex3 0.0 0.0 0.0] :: [Vertex3 GLfloat]
                 sigPrev1 = if vBufLen > 0 then (vBuf !! sigPrev1Idx) else [Vertex3 0.0 0.0 0.0] :: [Vertex3 GLfloat]
                 sigPrev2 = if vBufLen > 0 then (vBuf !! sigPrev2Idx) else [Vertex3 0.0 0.0 0.0] :: [Vertex3 GLfloat]
               
               -- Normal vector for current signal is incomplete: 
                 verticesAndNormalsCurr = RH.normalsFromVertices [sigPrev1,sigCurr,[]] 
               -- Normal vector for prev signal is complete: 
                 verticesAndNormalsPrev = RH.normalsFromVertices [sigPrev2,sigPrev1,sigCurr] 

                 updatedNormalsBuf = verticesAndNormalsCurr : verticesAndNormalsPrev : adjNormalsBuf 
  _ -> []

updateNormalsBuffer nBuf _ _ = nBuf
-- }}}

verticesFromSamples :: [ SigGen.SValue ] -> Int -> FFTSurface -> [ Vertex3 GLfloat ]
verticesFromSamples samples signalIdx visual = zipWith ( \xIdx s -> let x  = (xPosFun visual) xIdx visual
                                                                        y  = realToFrac $ (scaleFun visual) s signalIdx xIdx 
                                                                        z  = 0.0 in
                                                                     -- z = (zPosFun renderSettings) signalIdx renderSettings in
                                                                    Vertex3 x y z ) [0..] samples

-- Expects a 2-dimensional list of vertices, considering the first dimension a z-Index, 
-- and a function mapping z-Indices to z-coordinates. 
-- Updates z-coordinate in every vertex to (zPosFunc zIndex). 
updateVerticesZCoord :: [[ Vertex3 GLfloat ]] -> FFTSurface -> [[ Vertex3 GLfloat ]]
updateVerticesZCoord signalsVertices visual = zipWith (\sigVertices zIdx -> setSignalZVal sigVertices zIdx) signalsVertices [0..]
  where setSignalZVal vertexList z = map (\v -> Vertex3 (vx3x v) (vx3y v) (zCoord z) ) vertexList
        nSignals = length signalsVertices
        zPosFunc = zPosFun visual
        zCoord z = (zPosFunc (nSignals-1-z) visual )

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
      gMaterial    = AC.gridMaterial settings

  materialAmbient   FrontAndBack $= color4MulAlpha (AC.materialAmbient gMaterial) gOpacity
  materialDiffuse   FrontAndBack $= color4MulAlpha (AC.materialDiffuse gMaterial) gOpacity
  materialSpecular  FrontAndBack $= color4MulAlpha (AC.materialSpecular gMaterial) gOpacity
  materialEmission  FrontAndBack $= color4MulAlpha (AC.materialEmission gMaterial) gOpacity
  materialShininess FrontAndBack $= AC.materialShininess gMaterial
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
      sMaterial    = AC.surfaceMaterial settings
  
  materialAmbient   FrontAndBack $= color4MulAlpha (AC.materialAmbient sMaterial) sOpacity
  materialDiffuse   FrontAndBack $= color4MulAlpha (AC.materialDiffuse sMaterial) sOpacity
  materialSpecular  FrontAndBack $= color4MulAlpha (AC.materialSpecular sMaterial) sOpacity
  materialEmission  FrontAndBack $= color4MulAlpha (AC.materialEmission sMaterial) sOpacity
  materialShininess FrontAndBack $= AC.materialShininess sMaterial
-- }}}

-- Render surface as single strips (for z : for x). 
-- Expects pairs of current and next signal data, 
-- with signal data being vertices, normals, and signal features. 
renderSignalSurfaceStrip :: DirectionZ -> 
                            DirectionX -> 
                            ( FE.SignalFeatures -> FE.FeatureTarget -> AC.ContextSettings -> IO () ) -> 
                            (Array Int (Vertex3 GLfloat), Array Int (Vertex3 GLfloat)) -> 
                            (Array Int (Normal3 GLfloat), Array Int (Normal3 GLfloat)) -> 
                            (FE.SignalFeatures, FE.SignalFeatures) -> 
                            AC.ContextSettings -> 
                            Int -> -- signal index
                            IO ()
-- {{{
renderSignalSurfaceStrip dirZ dirX fAppFun (vsArrCurr,vsArrNext) (nsArrCurr,nsArrNext) (fsCurr,fsNext) settings sigIdx = (
  do -- Put vertices and normals in correct (interleaved) order for 
     -- rendendering: 
     let aMaxIdx array = snd $ bounds array
     let aMinIdx array = fst $ bounds array
     let aMap array = ixmap (0,aMaxIdx array) (\i -> colIdcs !! i) array
                      where colIdcs = if dirX == LeftToRight then ( 
                                        [(aMinIdx array)..(aMaxIdx array)] ) 
                                      else ( 
                                        [(aMaxIdx array) - x | x <- [(aMinIdx array)..(aMaxIdx array)] ] )
     let vsCurr = aMap vsArrCurr
     let vsNext = aMap vsArrNext
     let nsCurr = aMap nsArrCurr
     let nsNext = aMap nsArrNext
     -- Bottleneck here: If splitXEnd - splitXStart gets big, so do vsCurr and vsNext! 
     -- This is why we need Arrays for interleaving, not lists. Lists are really, really 
     -- slow in this case. 
     let sortedVertices = Conv.interleaveArrays vsNext vsCurr 
     let sortedNormals  = Conv.interleaveArrays nsNext nsCurr 
  
     let nSignals  = AC.signalBufferSize settings

     fAppFun fsCurr FE.LocalTarget settings
     fAppFun fsNext FE.LocalTarget settings
     renderPrimitive TriangleStrip ( 
       mapM_ vertexWithNormal (Conv.aZip sortedVertices sortedNormals) )
  )
-- }}}

renderSignalGridStrip :: DirectionZ -> 
                         DirectionX -> 
                         ( FE.SignalFeatures -> FE.FeatureTarget -> AC.ContextSettings -> IO () ) -> 
                         (Array Int (Vertex3 GLfloat), Array Int (Vertex3 GLfloat)) -> 
                         (Array Int (Normal3 GLfloat), Array Int (Normal3 GLfloat)) -> 
                         (FE.SignalFeatures, FE.SignalFeatures) -> 
                         AC.ContextSettings -> 
                         Int -> -- signal index
                         IO ()
renderSignalGridStrip dirZ dirX fAppFun (vsArrCurr,vsArrNext) (nsArrCurr,nsArrNext) (fsCurr,fsNext) settings sigIdx = (
-- {{{     
  do -- Put vertices and normals in correct (interleaved) order for 
     -- rendendering: 
     let aMaxIdx array = snd $ bounds array
     let aMinIdx array = fst $ bounds array
     let aMap array = ixmap (0,aMaxIdx array) (\i -> colIdcs !! i) array
                      where colIdcs = if dirX == LeftToRight then ( 
                                        [(aMinIdx array)..(aMaxIdx array)] ) 
                                      else ( 
                                        [(aMaxIdx array) - x | x <- [(aMinIdx array)..(aMaxIdx array)] ] )
     let vsCurr = aMap vsArrCurr
     let vsNext = aMap vsArrNext
     let nsCurr = aMap nsArrCurr
     let nsNext = aMap nsArrNext

     fAppFun fsCurr FE.LocalTarget settings
     fAppFun fsNext FE.LocalTarget settings

     -- Lines in X-direction: 
     renderPrimitive LineStrip (
       mapM_ vertexWithNormal (Conv.aZip vsNext nsNext) )
     {-
     -- Lines in Z-direction: 
     mapM_ (\((vc,nc),(vn,nn)) -> do renderPrimitive LineStrip ( do fAppFun fsCurr FE.LocalTarget settings
                                                                    vertexWithNormal (vc,nc) 
                                                                    fAppFun fsNext FE.LocalTarget settings
                                                                    vertexWithNormal (vn,nn) ) 
           ) ( zip (Conv.aZip vsCurr nsCurr) (Conv.aZip vsNext nsNext) )
     -}
  )
-- }}}

-- List of indices indicating the order in which vertices have to be drawn depending 
-- on Z-direction (BackToFront | FrontToBack). 
-- TODO: Check if there is a performance gain when creating final drawing indices here (by
--       depending on X-direction) instead of switching X-order in renderSignalSurfaceStrip. 
--
-- Example: This is a matrix of indices in the section of the surface we want to render. 
--   
--                x-split
--                   |
--     0  1  2  3  4 | x  x  x  x  x
--    10 11 12 13 14 | x  x  x  x  x 
--    20 21 22 23 24 | x  x  x  x  x
--    30 31 32 33 34 | x  x  x  x  x 
--    ---------------+-------------- <-- z-split
--     x  x  x  x  x | x  x  x  x  x 
--
--    Here, start index is (0,0) and end index is (34,4). 
--
--    Output is list of indices in order they will be rendered: 
--    -> [ 0, 10, 1, 11, 2, 12, 3, 13, 4, 14, 10, 20, 11, 21, ... ]
--
vertexSectionIndices :: DirectionX -> DirectionZ -> Int -> (Int,Int) -> (Int,Int) -> Array Int (Array Int Int)
-- {{{
vertexSectionIndices _ dirZ nSamples (zStartIdx,xStartIdx) (zEndIdx,xEndIdx) = listArray (0,((length list)-1)) $ map (\idcs -> listArray (0,((length idcs)-1)) idcs) list
  where absIndexBtF z x = (nSamples * z) + x 
        absIndexFtB z x = (nSamples * zEndIdx) + x - (nSamples * (z-zStartIdx))
        list = if dirZ == FrontToBack then (
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

      let nullVertex = Vertex3 0 0 (0::GLfloat)
      let nullNormal = Normal3 0 0 (0::GLfloat)
      let vSecArray = amap ( \zArray -> amap ( \i -> safeArrayAt vArray i nullVertex ) zArray ) vIndexMatrixZSorted
      let nSecArray = amap ( \zArray -> amap ( \i -> safeArrayAt nArray i nullNormal ) zArray ) vIndexMatrixZSorted

      -- Render single surface strips of this section in correct Z- and X-order: 
      mapM_ ( \(sigIdx,secSigIdx) -> ( do let globalSigIdx = zEndIdx
                                              vsCurr = safeArrayAt vSecArray secSigIdx (listArray (0,-1) [])
                                              vsNext = safeArrayAt vSecArray (secSigIdx+1) vsCurr
                                              nsCurr = safeArrayAt nSecArray secSigIdx (listArray (0,-1) [])
                                              nsNext = safeArrayAt nSecArray (secSigIdx+1) nsCurr
                                              fCurr  = safeArrayAt fArray sigIdx FE.emptyFeatures
                                              fNext  = safeArrayAt fArray sigIdx fCurr
                                              vsTpl  = if dirZ == FrontToBack then (vsCurr,vsNext) else (vsNext,vsCurr)
                                              nsTpl  = if dirZ == FrontToBack then (nsCurr,nsNext) else (nsNext,nsCurr)
                                              fTpl   = if dirZ == FrontToBack then (fCurr,fNext) else (fNext,fCurr)
                                          translate $ Vector3 0.0 (0.01) (0.0 :: GLfloat)
                                          renderSignalGridStrip dirZ dirX applyFeaturesToGrid vsTpl nsTpl fTpl settings globalSigIdx 
                                          translate $ Vector3 0.0 (-0.01) (0.0 :: GLfloat) 
                                          renderSignalSurfaceStrip dirZ dirX applyFeaturesToSurface vsTpl nsTpl fTpl settings globalSigIdx )
           ) (zip sigIdcs [0..nSecSignals])
  ) else return () 
-- }}}

-- Render surface as four sections, each with a different X- and Z-direction. 
renderSurface :: [[ Vertex3 GLfloat ]] -> 
                 [[ Normal3 GLfloat ]] -> 
                 [ FE.SignalFeatures ] -> 
                 Vector3 GLfloat -> 
                 Int -> 
                 AC.ContextSettings -> 
                 FFTSurface -> 
                 IO ()
-- {{{
renderSurface vBuf nBuf fBuf viewpoint nSamples settings visual = do
  let nSignals   = length vBuf
      vBufMaxIdx = (nSignals * nSamples) - 1
      nBufMaxIdx = ((length nBuf) * nSamples) - 1
      fBufMaxIdx = (length fBuf) - 1
      flatVBuf   = (concat vBuf)
      flatNBuf   = (concat nBuf)
      vArray     = listArray (0,vBufMaxIdx) flatVBuf
      nArray     = listArray (0,nBufMaxIdx) flatNBuf
      fArray     = listArray (0,fBufMaxIdx) fBuf

  let zCoordFun = zPosFun visual
      xCoordFun = xPosFun visual
      splitZ    = case findIndex ( \z -> (zCoordFun (z+1) visual) >= (v3z viewpoint) ) [0..(nSignals-1)] of 
                    Just idx -> nSignals-idx-1
                    Nothing  -> 0
      splitX    = case findIndex ( \x -> (xCoordFun x visual) >= (v3x viewpoint) ) [0..(nSamples-1)] of 
                    Just idx -> idx
                    Nothing  -> if (xCoordFun 0 visual) >= (v3x viewpoint) then 0 else (nSamples-1)
      -- splitZ = 15
      -- splitX = 65

  let secBottomLeftStartIdcs     = ( 0 :: Int, 0 :: Int)
      secBottomLeftEndIdcs       = ( splitZ, splitX )
      secBottomRightStartIdcs    = ( 0 :: Int, splitX )
      secBottomRightEndIdcs      = ( splitZ, (nSamples-1) )
      secTopLeftStartIdcs  = ( splitZ, 0 :: Int )
      secTopLeftEndIdcs    = ( (nSignals-1), splitX )
      secTopRightStartIdcs = ( splitZ, splitX )
      secTopRightEndIdcs   = ( (nSignals-1), (nSamples-1) )

  let renderSec dX dZ sS sE   = renderSurfaceSection dX dZ vArray nArray fArray sS sE nSamples settings
  
--  cullFace $= if v3y viewpoint > 0 then Just Back else Just Front
  
  -- translate $ Vector3 (-0.15) 0 (0.15::GLfloat)
  renderSec LeftToRight BackToFront secTopLeftStartIdcs secTopLeftEndIdcs 
  -- translate $ Vector3 (0.15) 0 (0.0::GLfloat)
  renderSec RightToLeft BackToFront secTopRightStartIdcs secTopRightEndIdcs 
  -- translate $ Vector3 (-0.15) 0 (0.15::GLfloat)
  renderSec LeftToRight FrontToBack secBottomLeftStartIdcs secBottomLeftEndIdcs 
  -- translate $ Vector3 (0.15) 0 (0.0::GLfloat)
  renderSec RightToLeft FrontToBack secBottomRightStartIdcs secBottomRightEndIdcs 
-- }}}

{- 
-- Applying IIR filter to vertex Y-coords is not used right now as 
-- IIR filters happen in signal transformation where they belong. 

vxIIR :: (Fractional a) => Vertex3 a -> [Vertex3 a] -> [a] -> Vertex3 a
vxIIR sample samples coefs = foldl (\ac (i,(Vertex3 x y z)) -> vx3add ac (Vertex3 x ((coefs !! i) * y) z)) (Vertex3 0.0 0.0 0.0) (zip [0..num-1] (sample : (take num samples)))
  where num = length coefs

vxApplyIIR :: (Fractional a) => [Vertex3 a] -> [a] -> Int -> [Vertex3 a]
vxApplyIIR samples coefs num =  if num > 0 then vxApplyIIR (headSamples ++ [iirVertex] ++ tailSamples) coefs (num-1) else samples
  where iirVertex   = vxIIR curVertex tailSamples coefs
        curVertex   = samples !! (num-1)
        headSamples = take (num-1) samples
        tailSamples = drop num samples
  
updateSignalVerticesIIR :: [ Vertex3 GLfloat ] -> [ Vertex3 GLfloat ] -> GLfloat -> [ Vertex3 GLfloat ]
updateSignalVerticesIIR sigVsPrev sigVs coef = zipWith ( \psv sv -> Vertex3 (vx3x sv) (vx3y sv * coef + vx3y psv * (1-coef)) (vx3z sv) ) sigVsPrev sigVs

updateVerticesIIR :: [[ Vertex3 GLfloat ]] -> Int -> GLfloat -> [[ Vertex3 GLfloat ]]
updateVerticesIIR vBuf num coef = (updateVerticesIIR' vBuf num coef) ++ (drop num vBuf)

updateVerticesIIR' :: [[ Vertex3 GLfloat ]] -> Int -> GLfloat -> [[ Vertex3 GLfloat ]]
updateVerticesIIR' vBuf num coef = if num > 0 then updatedSigs ++ [ ( updateSignalVerticesIIR lastOldSig sigToUpdate coef ) ] 
                                              else []
  where sigs        = (reverse (take (num+1) vBuf)) :: [[ Vertex3 GLfloat ]] -- [ old, new, new, ...]
        lastOldSig  = (sigs !! 0) :: [ Vertex3 GLfloat ]
        sigToUpdate = (sigs !! 1) :: [ Vertex3 GLfloat ]
        updatedSigs = (updateVerticesIIR' vBuf (num-1) coef)
-}

