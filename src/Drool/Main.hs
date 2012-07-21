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
import qualified Drool.Utils.Conversions as Conv
import qualified Drool.Utils.Transformation as Trans
import qualified Drool.Utils.FeatureExtraction as FE
import qualified Drool.ApplicationContext as AC

import qualified Drool.UI.Menubar as Menubar
import qualified Drool.UI.ViewOptions as ViewOptions
import qualified Drool.UI.FeatureExtractionOptions as FeatureExtractionOptions
import qualified Drool.UI.TransformationOptions as TransformationOptions
import qualified Drool.UI.SignalBufferOptions as SignalBufferOptions
import qualified Drool.UI.SignalSourceOptions as SignalSourceOptions
import qualified Drool.UI.GLWindow as GLWindow

import qualified Sound.Pulse.Simple as Pulse
import qualified Control.Monad as M
import qualified Control.Concurrent as C
import qualified Control.Concurrent.Chan as CC
import qualified Control.Concurrent.MVar as MV ( MVar, newMVar, readMVar, tryPutMVar, tryTakeMVar, swapMVar, takeMVar, putMVar )

import Data.Array.IO ( IOUArray )

data SamplingState = SamplingPaused | SamplingActive
  deriving ( Eq, Show ) 

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
  contextObjects <- newIORef (
    AC.ContextObjects { AC.samplingThreadId  = undefined, 
                        AC.samplingSem       = initSamplingSem, 
                        AC.numNewSignalsChan = numNewSignalsChan, 
                        AC.renderingSem      = initRenderingSem, 
                        AC.signalBuf         = signalBuffer,
                        AC.featuresBuf       = signalFeaturesBuffer, 
                        AC.signalGenerator   = defaultSiggen } ) 

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
  _ <- ViewOptions.initComponent builder contextSettings contextObjects
  _ <- SignalSourceOptions.initComponent builder contextSettings contextObjects
  _ <- TransformationOptions.initComponent builder contextSettings contextObjects
  _ <- FeatureExtractionOptions.initComponent builder contextSettings contextObjects

  objects  <- readIORef contextObjects
  settings <- readIORef contextSettings

  let sampleRate = AC.audioSampleRate settings -- samples per second, usually 191000. 
  soundSource <- Pulse.simpleNew Nothing "DroolRecord" Pulse.Record Nothing "Drool audio visualizer" 
                   (Pulse.SampleSpec (Pulse.F32 Pulse.LittleEndian) (sampleRate) 1) Nothing (Just (Pulse.BufferAttr (Just (-1)) Nothing Nothing Nothing (Just 0)))
  soundTarget <- Pulse.simpleNew Nothing "DroolPlayback" Pulse.Play Nothing "Drool audio playback" 
                   (Pulse.SampleSpec (Pulse.F32 Pulse.LittleEndian) (sampleRate) 1) Nothing Nothing
  
  sampleTickIORef    <- newIORef 0
  sampleThread       <- C.forkOS . M.forever $ do cSettings   <- readIORef contextSettings
                                                  cObjects    <- readIORef contextObjects
                                                  let sigGen        = AC.signalGenerator cObjects
                                                  let numSamples    = (SigGen.numSamples sigGen)
                                                  let bufferMaxSize = AC.signalBufferSize cSettings
                                                  let ampSignal s   = if (AC.ampEnabled cSettings) then s * (realToFrac $ AC.signalAmpDb cSettings) else s
                                                  let fftWindowSize = AC.numFFTBands cSettings
                                                  let readjustBufferSize buf maxSize = if length buf > maxSize then take maxSize buf else buf
                                                  -- If sampling microphone input, read it: 
                                                  soundSamples <- Pulse.simpleRead soundSource $ fftWindowSize :: IO[Float]
                                                  latency <- Pulse.simpleGetLatency soundSource
                                                  putStrLn $ "Latency: " ++ show latency
                                                  -- If using a test signal, generate it: 
                                                  sampleTick <- readIORef sampleTickIORef 
                                                  modifyIORef sampleTickIORef (\tick -> (tick + 1) `mod` 1000)
                                                  let testSignalSamples = take numSamples (SigGen.genSignal sigGen sampleTick)
                                                  
                                                  let rawSamples = if AC.signalSource cSettings == DT.Microphone then soundSamples else map (\v -> realToFrac v) testSignalSamples

                                                  let playback = if AC.playbackEnabled cSettings then Pulse.simpleWrite soundTarget rawSamples else return ()
                                                  -- Transform: FFT
                                                  fftTransformedSamples <- if AC.fftEnabled cSettings then Trans.fftwFloats rawSamples else return rawSamples
                                                  -- Transform: Amp
                                                  let amplifiedSamples = map (\x -> ampSignal $ realToFrac x) $ take numSamples fftTransformedSamples
                                                  -- Feature Extraction: 
                                                  let feSettings = FE.FeatureExtractionSettings { FE.maxBeatBand = (AC.maxBeatBand cSettings) }
                                                  let newFeatures = FE.extractSignalFeatures amplifiedSamples feSettings sigGen
                                                  -- Push signal: 
                                                  newSignal <- (newListArray (0, length amplifiedSamples - 1) amplifiedSamples)::IO (IOUArray Int Float)
                                                  
                                                  signalBuf <- readIORef signalBuffer
                                                  let newSignalBuf = DT.CSignalList ( readjustBufferSize ([DT.CSignal newSignal] ++ (DT.signalList signalBuf)) bufferMaxSize )
                                                  
                                                  iirFilteredSignals <- if AC.iirEnabled cSettings then Trans.signalIIR newSignalBuf 1 (realToFrac $ AC.iirCoef cSettings) else return newSignalBuf

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
   
                                                  let timeoutMs = (Conv.freqToMs $ AC.signalPushFrequency cSettings)
                                                  -- C.threadDelay timeoutMs
                                                  
                                                  _ <- playback
                                                  return () 

  -- transformationThread <- C.forkIO . forever $ do
                                                
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
