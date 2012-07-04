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

-- Main controller of the application, managing IORefs between ViewOptions
-- and rendering display.

{-# OPTIONS -O2 -Wall #-}

module Main where

import Data.IORef
import Data.Array.IO

import Graphics.Rendering.OpenGL ( ( $=!), GLfloat, Color3(..) )
import qualified Graphics.UI.Gtk as Gtk
import qualified Graphics.UI.Gtk.Builder as GtkBuilder

import qualified Drool.Types as DT
import qualified Drool.Utils.SigGen as SigGen
import qualified Drool.Utils.Conversions as Conv
import qualified Drool.Utils.FFT as FFT
import qualified Drool.ApplicationContext as AC

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

main :: IO()
main = do
--  _ <- Gtk.initGUI
  _ <- Gtk.unsafeInitGUIForThreadedRTS

  let signalBufferSize = 50
  emptySignal <- DT.newSignal
  -- Initialize signal buffer with signalBufferSize empty signals:
  let emptySignalBuffer = (DT.newSignalList (signalBufferSize::Int) emptySignal)
  signalBuffer <- newIORef (DT.CSignalList emptySignalBuffer)
  -- Initialize test signal generator:
  let transform pLength t sample = (SigGen.sine pLength t) * sample
  let defaultSiggen = SigGen.CSignalGenerator { SigGen.baseSignal = SigGen.CBaseSignal SigGen.dirac,
                                                SigGen.ampTransformation = SigGen.CAmpTransformation transform,
                                                SigGen.signalPeriodLength = 3,
                                                SigGen.envelopePeriodLength = 40,
                                                SigGen.numSamples = 200 }

  contextSettings <- newIORef(
    AC.ContextSettings { AC.samplingThreadId = undefined, 
                         AC.signalPushFrequency = 120,
                         AC.renderingFrequency = 30,
                         AC.signalBufferSize = signalBufferSize,
                         AC.fixedRotation = DT.CRotationVector { DT.rotX = 0.0::GLfloat, DT.rotY = 0.0::GLfloat, DT.rotZ = 0.0::GLfloat },
                         AC.incRotation = DT.CRotationVector { DT.rotX = 0.0::GLfloat, DT.rotY = 0.0::GLfloat, DT.rotZ = 0.0::GLfloat },
                         AC.incRotationAccum = DT.CRotationVector { DT.rotX = 0.0::GLfloat, DT.rotY = 0.0::GLfloat, DT.rotZ = 0.0::GLfloat },
                         AC.scaling = 30,
                         AC.rangeAmps = [ 1.0, 1.0, 1.0, 1.0, 1.0 ], 
                         AC.gridOpacity = 15,
                         AC.surfaceOpacity = 13,
                         AC.surfaceColor = Color3 (62.0/255) (187.0/255) (1::GLfloat),
                         AC.lightColor = Color3 (239.0/255) (19.0/255) (19.0/255.0::GLfloat) ,
                         AC.gridColor = Color3 (142.0/255) 1 (58.0/255::GLfloat),
                         AC.renderPerspective = DT.Isometric,
                         AC.maxBeatBandSamples = 20, 
                         AC.signalBuf = signalBuffer,
                         AC.signalGenerator = defaultSiggen } )

  -- Load UI configuration from GtkBuilder file:
  builder <- GtkBuilder.builderNew
  GtkBuilder.builderAddFromFile builder "droolui-test.glade"

  -- Instatiate window from GtkBuilder file:
  mainWindow <- GtkBuilder.builderGetObject builder Gtk.castToWindow "mainWindow"


  openVisualizerButton <- GtkBuilder.builderGetObject builder Gtk.castToButton "buttonVisualizer"
  _ <- Gtk.onClicked openVisualizerButton $ do
    GLWindow.initComponent builder contextSettings

  -- Application exit callback (quits main loop):
  _ <- Gtk.onDestroy mainWindow Gtk.mainQuit

  -- Define button callbacks:
  menuItem_FileClose <- GtkBuilder.builderGetObject builder Gtk.castToMenuItem "menuitemFileQuit"
  _ <- Gtk.on menuItem_FileClose Gtk.menuItemActivate $ do
    putStrLn "Exiting"
    Gtk.widgetDestroy mainWindow

  _ <- SignalBufferOptions.initComponent builder contextSettings
  _ <- ViewOptions.initComponent builder contextSettings
  _ <- SignalSourceOptions.initComponent builder contextSettings
  _ <- TransformationOptions.initComponent builder contextSettings
  _ <- FeatureExtractionOptions.initComponent builder contextSettings

  let sampleRate = 191000 -- samples per second
  let fftRes     = 10240  -- 4096 * 2 -- how many samples to use for FFT
  soundSource <- Pulse.simpleNew Nothing "DroolRecord" Pulse.Record Nothing "Drool audio visualizer" 
                   (Pulse.SampleSpec (Pulse.F32 Pulse.LittleEndian) (sampleRate) 1) Nothing (Just (Pulse.BufferAttr (Just (-1)) Nothing Nothing Nothing (Just 0)))
  soundTarget <- Pulse.simpleNew Nothing "DroolPlayback" Pulse.Play Nothing "Drool audio playback" 
                   (Pulse.SampleSpec (Pulse.F32 Pulse.LittleEndian) (sampleRate) 1) Nothing Nothing

  sampleChan <- CC.newChan 
  sampleThread <- C.forkOS . M.forever $ do soundSamples <- Pulse.simpleRead soundSource $ fftRes :: IO[Float]
                                            -- TODO: Enable if playback flag is set
                                            Pulse.simpleWrite soundTarget soundSamples
                                            let fftSamples = (FFT.fftFloats soundSamples)
                                            fftSamples <- FFT.fftwFloats soundSamples
                                            cSettings <- readIORef contextSettings
                                            let siggen = AC.signalGenerator cSettings
                                            let numChanSamples = SigGen.numSamples siggen
                                            CC.writeChan sampleChan (take numChanSamples (fftSamples))
                                            -- CC.writeChan sampleChan (take numChanSamples soundSamples)
                                            -- Pulse.simpleDrain soundTarget
  -- settings <- readIORef contextSettings
  -- contextSettings $=! settings { AC.samplingThreadId = sampleThread }
 
  let updateCallback count = (do

      cSettings <- readIORef contextSettings

      let siggen = AC.signalGenerator cSettings

      -- let genSampleList = take (SigGen.numSamples siggen) (SigGen.genSignal siggen count)
      sigsamples <- CC.readChan sampleChan

      let scale s = s * 5.0 -- ((sqrt (s+13.0)) - 3.6) * 5.0
      let genSampleList = map (\x -> scale (realToFrac x) ) $ take (SigGen.numSamples siggen) sigsamples

      -- mapM_ (\x -> putStrLn $ show x) genSampleList
      
      -- Sample list to array:
      newSignal <- (newListArray (0, length genSampleList - 1) genSampleList)::IO (IOArray Int GLfloat)
      -- pop first signal in buffer if buffer is full:
      let bufferMaxSize = AC.signalBufferSize cSettings
      let readjustBufferSize buf maxSize = if length buf > maxSize then drop (length buf - maxSize) buf else buf
      -- Push new signal to buffer:
      modifyIORef signalBuffer (\list -> DT.CSignalList(
                                           ((readjustBufferSize (DT.signalList list) bufferMaxSize)
                                         ++ [ DT.CSignal newSignal ]) ))

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

  -- Initialize sample timer with t=0 and start immediately:
  updateSamplesTimer <- Gtk.timeoutAddFull (updateCallback 0) Gtk.priorityDefaultIdle 1

  -- Remove sample timer when closing application:
  _ <- Gtk.onDestroy mainWindow (Gtk.timeoutRemove updateSamplesTimer)

  -- Display window:
  _ <- Gtk.widgetShowAll mainWindow

  -- Enter GUI main loop:
  Gtk.mainGUI

  -- After main loop, when exiting application: 

  -- Terminate sampling thread: 
  -- TODO

  -- Free playback sound buffer
  -- Pulse.simpleDrain soundTarget
  Pulse.simpleFree soundTarget

  return ()
