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
  
  sampleChan       <- CC.newChan 
  sampleThread     <- C.forkOS . M.forever $ do sampleState <- readIORef samplingStateIORef
                                                cSettings   <- readIORef contextSettings
                                                let loop = if sampleState == SamplingPaused then ( 
                                                             C.threadDelay 10 )
                                                           else ( do
                                                             soundSamples <- Pulse.simpleRead soundSource $ (AC.numFFTBands cSettings) :: IO[Float]
                                                             -- TODO: Enable if playback flag is set
                                                             Pulse.simpleWrite soundTarget soundSamples
                                                             renderSamples <- if AC.fftEnabled cSettings then FFT.fftwFloats soundSamples else return soundSamples
                                                             cObjects <- readIORef contextObjects
                                                             let siggen = AC.signalGenerator cObjects
                                                             let numChanSamples = SigGen.numSamples siggen
                                                             CC.writeChan sampleChan (take numChanSamples (renderSamples)) )
                                                loop
                                                
  contextObjects $=! objects { AC.samplingThreadId = sampleThread }
 
  let updateCallback count = (do
  -- {{{
      cSettings     <- readIORef contextSettings
      cObjects      <- readIORef contextObjects
      samplingState <- readIORef samplingStateIORef

      -- Get active signal generator from context objects: 
      let siggen = AC.signalGenerator cObjects
      -- Get samples from sampling thread: 
      sigsamples <- if samplingState == SamplingActive then CC.readChan sampleChan else return []

      -- Helper function. Pop first signal in buffer if buffer is full. 
      let readjustBufferSize buf maxSize = if length buf > maxSize then drop (length buf - maxSize) buf else buf
      
      -- If using a test signal, generate it: 
      let genSampleListTestSignal = take (SigGen.numSamples siggen) (SigGen.genSignal siggen count)
      -- If sampling microphone input, read it: 
      let ampSignal s = if (AC.ampEnabled cSettings) then s * (realToFrac $ AC.signalAmpDb cSettings) else s
      let genSampleListMicrophone = map (\x -> ampSignal (realToFrac x) ) $ take (SigGen.numSamples siggen) sigsamples
      let genSampleList = if AC.signalSource cSettings == DT.Microphone then genSampleListMicrophone else genSampleListTestSignal

      let toggleSampling source state = case source of 
                                          DT.Microphone -> if state == SamplingPaused then 
                                                              atomicModifyIORef samplingStateIORef ( \_ -> (SamplingActive, True) ) >>= return
                                                           else return (False)
                                          DT.TestSignal -> if state == SamplingActive then 
                                                              atomicModifyIORef samplingStateIORef ( \_ -> (SamplingPaused, True) ) >>= return
                                                           else return (False)
                                          _ -> return False 
      samplingToggled <- toggleSampling (AC.signalSource cSettings) samplingState

      -- Get max size of buffers: 
      let bufferMaxSize = AC.signalBufferSize cSettings

      -- Extract features from current signal buffer: 
      let feSettings = FE.FeatureExtractionSettings { FE.maxBeatBand = (AC.maxBeatBand cSettings) }
      let features   = FE.extractSignalFeatures genSampleList feSettings siggen
      -- Push new features to buffer: 
      _ <- atomicModifyIORef signalFeaturesBuffer (\list -> ( FE.SignalFeaturesList( readjustBufferSize ((FE.signalFeaturesList list) ++ [features] ) bufferMaxSize), True ) )

      -- Sample list to array:
      newSignal <- (newListArray (0, length genSampleList - 1) genSampleList)::IO (IOArray Int GLfloat)
      -- Push new signal to buffer:
      _ <- atomicModifyIORef signalBuffer (\list -> ( DT.CSignalList( readjustBufferSize ((DT.signalList list) ++ [DT.CSignal newSignal]) bufferMaxSize), True ) )

      -- Increment values of incremental rotation: 
      let accIncRotation  = (AC.incRotationAccum cSettings) 
      let incRotationStep = (AC.incRotation cSettings) 
      let nextIncRotation = DT.CRotationVector { DT.rotY = (DT.rotY accIncRotation + DT.rotY incRotationStep), 
                                                 DT.rotX = (DT.rotX accIncRotation + DT.rotX incRotationStep), 
                                                 DT.rotZ = (DT.rotZ accIncRotation + DT.rotZ incRotationStep) } 
      contextSettings $=! cSettings { AC.incRotationAccum = nextIncRotation }
      
      -- Start a new timeout with incremented count:
      let timeoutMs = (Conv.freqToMs $ AC.signalPushFrequency cSettings)
      _ <- Gtk.timeoutAddFull (updateCallback (count+1)) Gtk.priorityHighIdle timeoutMs

      -- do not run this callback again:
      return False)
  -- }}}
  
  -- Initialize sample timer with t=0 and start immediately:
  updateSamplesTimer <- Gtk.timeoutAddFull (updateCallback 0) Gtk.priorityDefaultIdle 0

  -- Remove sample timer when closing application:
  _ <- Gtk.onDestroy mainWindow (Gtk.timeoutRemove updateSamplesTimer)

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
