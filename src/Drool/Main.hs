-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :
-- License     :  GPL v3
--
-- Maintainer  :  Tobias Fuchs
-- Stability   :  unstable
-- Portability :  Win32, POSIX
--
-- |
--
-----------------------------------------------------------------------------

{-# OPTIONS -O2 -Wall #-}

module Main where

import Debug.Trace

import Data.IORef
import Data.Array.IO

import Graphics.Rendering.OpenGL ( ( $=!), GLfloat ) 
import qualified Graphics.UI.Gtk as Gtk
import qualified Graphics.UI.Gtk.Builder as GtkBuilder

import qualified Drool.Types as DT
import qualified Drool.Utils.SigGen as SigGen
import qualified Drool.Utils.Transformation as Trans
import qualified Drool.Utils.FeatureExtraction as FE
import qualified Drool.ApplicationContext as AC
import qualified Drool.ContextObjects as AC

import qualified Drool.UI.Menubar as Menubar
import qualified Drool.UI.ViewOptions as ViewOptions
import qualified Drool.UI.VisualOptions as VisualOptions
import qualified Drool.UI.FeatureExtractionOptions as FeatureExtractionOptions
import qualified Drool.UI.TransformationOptions as TransformationOptions
import qualified Drool.UI.SignalBufferOptions as SignalBufferOptions
import qualified Drool.UI.SignalSourceOptions as SignalSourceOptions
import qualified Drool.UI.Metrics as Metrics
import qualified Drool.UI.GLWindow as GLWindow
import qualified Drool.UI.Visuals as Visuals

import qualified Sound.Pulse.Simple as Pulse
import qualified Control.Monad as M
import qualified Control.Concurrent as C
import qualified Control.Concurrent.Chan as CC
import qualified Control.Concurrent.MVar as MV ( MVar, newMVar, readMVar, tryPutMVar, tryTakeMVar, swapMVar, takeMVar, putMVar )

import Data.Array.IO ( IOUArray )


main :: IO()
main = do

  _ <- Gtk.unsafeInitGUIForThreadedRTS -- Hell yeah

  -- Initialize signal buffer with signalBufferSize empty signals:
  signalBuffer <- newIORef (DT.CSignalList [])
  -- Initialize test signal generator:
  let transform pLength t sample = (SigGen.sine pLength t) * sample
  let defaultSiggen = SigGen.CSignalGenerator { SigGen.baseSignal           = SigGen.CBaseSignal SigGen.dirac,
                                                SigGen.ampTransformation    = SigGen.CAmpTransformation transform,
                                                SigGen.signalPeriodLength   = 3,
                                                SigGen.envelopePeriodLength = 40,
                                                SigGen.numSamples           = 130 }

  signalFeaturesBuffer <- newIORef (FE.SignalFeaturesList [])

  initSamplingSem  <- C.newMVar 0
  initRenderingSem <- C.newEmptyMVar 

  numNewSignalsChan <- CC.newChan 

  contextSettings <- newIORef ( AC.defaultContextSettings )

  visualInitState      <- Visuals.newFFTSurfaceState contextSettings 
  visualInitStateIORef <- newIORef visualInitState
  visualIORef <- newIORef $ Visuals.newFFTSurfaceVisual contextSettings visualInitStateIORef

  visualForegroundIORef <- newIORef $ Visuals.newBlankVisual 
  visualBackgroundIORef <- newIORef $ Visuals.newBlankVisual 

  -- Load a concrete visual definition (e.g. Visual FFTSurface): 
  contextObjects  <- newIORef (
    AC.ContextObjects { AC.visualForeground        = visualForegroundIORef, 
                        AC.visualMiddleground      = visualIORef, 
                        AC.visualBackground        = visualBackgroundIORef, 
                        AC.samplingThreadId        = undefined, 
                        AC.samplingSem             = initSamplingSem, 
                        AC.numNewSignalsChan       = numNewSignalsChan, 
                        AC.renderingSem            = initRenderingSem, 
                        AC.signalBuf               = signalBuffer,
                        AC.featuresBuf             = signalFeaturesBuffer, 
                        AC.signalGenerator         = defaultSiggen, 
                        AC.visualDefinitionChanged = True } ) 

  -- Load UI configuration from GtkBuilder file:
  builder <- GtkBuilder.builderNew
  GtkBuilder.builderAddFromFile builder "droolui-test.glade"

  -- Instatiate window from GtkBuilder file:
  mainWindow <- GtkBuilder.builderGetObject builder Gtk.castToWindow "mainWindow"
  
  openVisualizerButton <- GtkBuilder.builderGetObject builder Gtk.castToButton "buttonVisualizer"
  _ <- Gtk.onClicked openVisualizerButton $ do
    GLWindow.initComponent builder contextSettings contextObjects

  -- Application exit callback (quits main loop):
  _ <- Gtk.onDestroy mainWindow Gtk.mainQuit

  -- Initialize all GUI components: 
  _ <- Menubar.initComponent builder contextSettings contextObjects
  _ <- SignalBufferOptions.initComponent builder contextSettings contextObjects
  _ <- VisualOptions.initComponent builder contextSettings contextObjects
  _ <- ViewOptions.initComponent builder contextSettings contextObjects
  _ <- SignalSourceOptions.initComponent builder contextSettings contextObjects
  _ <- TransformationOptions.initComponent builder contextSettings contextObjects
  _ <- FeatureExtractionOptions.initComponent builder contextSettings contextObjects
  _ <- Metrics.initComponent builder contextSettings contextObjects

  objects  <- readIORef contextObjects
  settings <- readIORef contextSettings

  let sampleRate = AC.audioSampleRate settings -- samples per second, usually 191000. 
  soundSource <- Pulse.simpleNew Nothing "DroolRecord" Pulse.Record Nothing "Drool audio visualizer" 
                   (Pulse.SampleSpec (Pulse.F32 Pulse.LittleEndian) (sampleRate) 1) Nothing (Just (Pulse.BufferAttr (Just (-1)) Nothing Nothing Nothing (Just 0)))
  soundTarget <- Pulse.simpleNew Nothing "DroolPlayback" Pulse.Play Nothing "Drool audio playback" 
                   (Pulse.SampleSpec (Pulse.F32 Pulse.LittleEndian) (sampleRate) 1) Nothing Nothing
  
  transformationChan <- CC.newChan

  putStrLn "Draining playback buffer ..."
  Pulse.simpleDrain soundTarget
  putStrLn "Flushing playback buffer ..."
  Pulse.simpleFlush soundTarget

  sampleTickIORef <- newIORef 0
  sampleThread    <- C.forkOS . M.forever $ do cSettings   <- readIORef contextSettings
                                               cObjects    <- readIORef contextObjects
                                               let sigGen        = AC.signalGenerator cObjects
                                               let numSamples    = (SigGen.numSamples sigGen)
                                               let bufferMaxSize = AC.signalBufferSize cSettings
                                               let fftWindowSize = AC.numFFTBands cSettings
                                               -- If sampling microphone input, read it: 
                                               let soundSamples = Pulse.simpleRead soundSource $ fftWindowSize :: IO[Float]
                                               -- If using a test signal, generate it: 
                                               modifyIORef sampleTickIORef (\tick -> tick + 1)
                                               sampleTick <- readIORef sampleTickIORef 
                                               let testSignalSamples = take numSamples (SigGen.genSignal sigGen sampleTick)
                                               rawSamples <- if AC.signalSource cSettings == DT.Microphone then soundSamples else return $ map (\v -> realToFrac v) testSignalSamples
                                               let playback = if AC.playbackEnabled cSettings then Pulse.simpleWrite soundTarget rawSamples else return ()
                                               _ <- playback
                                               
                                               CC.writeChan transformationChan rawSamples

                                               latency <- Pulse.simpleGetLatency soundSource
                                               modifyIORef contextSettings ( \s -> s { AC.metrics = (AC.metrics s) { AC.latency = latency } } )
                                               return () 

  transformThread <- C.forkIO . M.forever $ do cSettings   <- readIORef contextSettings
                                               cObjects    <- readIORef contextObjects
                                               let sigGen        = AC.signalGenerator cObjects
                                               let numSamples    = (SigGen.numSamples sigGen)
                                               let bufferMaxSize = AC.signalBufferSize cSettings
                                               let fftWindowSize = AC.numFFTBands cSettings
                                               let ampSignal s   = if (AC.ampEnabled cSettings) then s * (realToFrac $ AC.signalAmpDb cSettings) else s
                                               let readjustBufferSize buf maxSize = if length buf > maxSize then take maxSize buf else buf

                                               rawSamples <- CC.readChan transformationChan

                                               -- Transform: FFT
                                               fftTransformedSamples <- if AC.fftEnabled cSettings then Trans.fftwFloats rawSamples else return rawSamples
                                               -- Transform: Amp
                                               let amplifiedSamples = map (\x -> ampSignal $ realToFrac x) $ take numSamples fftTransformedSamples
                                               -- Feature Extraction: 
                                               let feSettings = FE.FeatureExtractionSettings { FE.maxBeatBand = (AC.maxBeatBand cSettings) }
                                               -- let newFeatures = FE.extractSignalFeatures amplifiedSamples feSettings sigGen
                                               -- Push signal: 
                                               newSignal <- (newListArray (0, length amplifiedSamples - 1) amplifiedSamples)::IO (IOUArray Int Float)
                                               
                                               signalBuf <- readIORef signalBuffer
                                               let newSignalBuf = DT.CSignalList ( readjustBufferSize ([DT.CSignal newSignal] ++ (DT.signalList signalBuf)) bufferMaxSize )
                                               
                                               iirFilteredSignals <- if AC.iirEnabled cSettings 
                                                                     then Trans.signalIIR newSignalBuf 1 (realToFrac $ AC.iirCoef cSettings) 
                                                                     else return newSignalBuf
                                               latestIIRSignal <- getElems $ DT.signalArray (head (DT.signalList iirFilteredSignals))
                                               let newFeatures = FE.extractSignalFeatures latestIIRSignal feSettings sigGen

                                               let iirTransformedSignalBuf = DT.CSignalList ( DT.signalList iirFilteredSignals ) 

                                               featuresBuf <- readIORef signalFeaturesBuffer
                                               let newFeaturesBuf = FE.SignalFeaturesList ( readjustBufferSize ([newFeatures] ++ (FE.signalFeaturesList featuresBuf)) bufferMaxSize )
                                               
                                               _ <- atomicModifyIORef signalBuffer ( \_ -> (iirTransformedSignalBuf, True) )
                                               _ <- atomicModifyIORef signalFeaturesBuffer ( \_ -> (newFeaturesBuf, True) )
                                               
                                               let samplingSem  = AC.samplingSem cObjects
                                               -- Do not wait on the MVar, only update it. The rendering thread 
                                               -- is waiting on it so it only renders on new signals. 
                                               semValue <- MV.tryTakeMVar samplingSem
                                               MV.tryPutMVar samplingSem ( case semValue of 
                                                                           Nothing -> 1
                                                                           Just v  -> (v+1) )
                                               return ()
                                                
  contextObjects $=! objects { AC.samplingThreadId = sampleThread }
 
  -- Remove sample timer when closing application:
  -- _ <- Gtk.onDestroy mainWindow (Gtk.timeoutRemove updateSamplesTimer)

  -- Display window:
  _ <- Gtk.widgetShowAll mainWindow

  -- Enter GUI main loop:
  Gtk.mainGUI

  -- After main loop, when exiting application: 
  -- Free playback sound buffer
  putStrLn "Closing PulseAudio buffer ..."
  -- Pulse.simpleDrain soundTarget
  Pulse.simpleFree soundTarget

  return ()
