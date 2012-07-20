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

import Data.IORef
import Data.Array.IO

import Graphics.Rendering.OpenGL ( ( $=!), GLfloat ) 
import qualified Graphics.UI.Gtk as Gtk
import qualified Graphics.UI.Gtk.Builder as GtkBuilder

import qualified Drool.Types as DT
import qualified Drool.Utils.SigGen as SigGen
import qualified Drool.Utils.Conversions as Conv
import qualified Drool.Utils.FFT as FFT
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


data SamplingState = SamplingPaused | SamplingActive
  deriving ( Eq, Show ) 

main :: IO()
main = do

  _ <- Gtk.unsafeInitGUIForThreadedRTS -- Hell yeah

  let signalBufferSize = 50
  emptySignal <- DT.newSignal
  -- Initialize signal buffer with signalBufferSize empty signals:
  let emptySignalBuffer = (DT.newSignalList (signalBufferSize::Int) emptySignal)
  signalBuffer <- newIORef (DT.CSignalList emptySignalBuffer)
  -- Initialize test signal generator:
  let transform pLength t sample = (SigGen.sine pLength t) * sample
  let defaultSiggen = SigGen.CSignalGenerator { SigGen.baseSignal           = SigGen.CBaseSignal SigGen.dirac,
                                                SigGen.ampTransformation    = SigGen.CAmpTransformation transform,
                                                SigGen.signalPeriodLength   = 3,
                                                SigGen.envelopePeriodLength = 40,
                                                SigGen.numSamples           = 130 }

  signalFeaturesBuffer <- newIORef (FE.SignalFeaturesList [])

  contextSettings <- newIORef ( AC.defaultContextSettings )
  contextObjects <- newIORef (
    AC.ContextObjects { AC.samplingThreadId = undefined, 
                        AC.signalBuf        = signalBuffer,
                        AC.featuresBuf      = signalFeaturesBuffer, 
                        AC.signalGenerator  = defaultSiggen } ) 

  samplingStateIORef <- newIORef SamplingActive

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
  
  sampleChan      <- CC.newChan 
  sampleTickIORef <- newIORef 0
  sampleThread    <- C.forkOS . M.forever $ do sampleState <- readIORef samplingStateIORef
                                               cSettings   <- readIORef contextSettings
                                               cObjects    <- readIORef contextObjects
                                               let bufferMaxSize = AC.signalBufferSize cSettings
                                               let siggen = AC.signalGenerator cObjects
                                               let ampSignal s = if (AC.ampEnabled cSettings) then s * (realToFrac $ AC.signalAmpDb cSettings) else s
                                               let readjustBufferSize buf maxSize = if length buf > maxSize then drop (length buf - maxSize) buf else buf
                                               let fftWindowSize = AC.numFFTBands cSettings
                                               let loop = ( do
                                                            soundSamples <- Pulse.simpleRead soundSource $ fftWindowSize :: IO[Float]
                                                            -- If using a test signal, generate it: 
                                                            sampleTick <- readIORef sampleTickIORef 
                                                            modifyIORef sampleTickIORef (\tick -> (tick + 1) `mod` 1000)
                                                            let testSignalSamples = take (SigGen.numSamples siggen) (SigGen.genSignal siggen sampleTick)
                                                            -- If sampling microphone input, read it: 
                                                            let rawSamples = if AC.signalSource cSettings == DT.Microphone then soundSamples else map (\v -> realToFrac v) testSignalSamples
                                                            -- TODO: Enable if playback flag is set
                                                            -- Pulse.simpleWrite soundTarget soundSamples
                                                            Pulse.simpleWrite soundTarget rawSamples
                                                            transformedSamples <- if AC.fftEnabled cSettings then FFT.fftwFloats rawSamples else return rawSamples
                                                            let sigGen = AC.signalGenerator cObjects
                                                            let numChanSamples = SigGen.numSamples sigGen
                                                            -- CC.writeChan sampleChan (take numChanSamples (renderSamples)) )
                                                            let feSettings = FE.FeatureExtractionSettings { FE.maxBeatBand = (AC.maxBeatBand cSettings) }
                                                            let amplifiedSamples = map (\x -> ampSignal $ realToFrac x) $ take (SigGen.numSamples siggen) transformedSamples
                                                            let features   = FE.extractSignalFeatures amplifiedSamples feSettings siggen
                                                            _ <- atomicModifyIORef signalFeaturesBuffer (\list -> ( FE.SignalFeaturesList( readjustBufferSize ((FE.signalFeaturesList list) ++ [features] ) bufferMaxSize), True ) )
                                                            newSignal <- (newListArray (0, length amplifiedSamples - 1) amplifiedSamples)::IO (IOArray Int GLfloat)
                                                            _ <- atomicModifyIORef signalBuffer (\list -> ( DT.CSignalList( readjustBufferSize ((DT.signalList list) ++ [DT.CSignal newSignal]) bufferMaxSize), True ) ) 
                                                            C.threadDelay (AC.signalPushFrequency cSettings)
                                                            return () )
                                               loop
                                                
  contextObjects $=! objects { AC.samplingThreadId = sampleThread }
 
  let updateCallback count = (do
  -- {{{
      cSettings     <- readIORef contextSettings
      cObjects      <- readIORef contextObjects
      samplingState <- readIORef samplingStateIORef
      
      -- Start a new timeout with incremented count:
      let timeoutMs = (Conv.freqToMs $ AC.signalPushFrequency cSettings)
      _ <- Gtk.timeoutAddFull (updateCallback (count+1)) Gtk.priorityHighIdle timeoutMs

      -- do not run this callback again:
      return False)
  -- }}}
  
  -- Initialize sample timer with t=0 and start immediately:
  -- updateSamplesTimer <- Gtk.timeoutAddFull (updateCallback 0) Gtk.priorityDefaultIdle 0

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
