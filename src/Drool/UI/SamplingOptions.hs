-----------------------------------------------------------------------------
--
-- Module      :  SamplingOptions
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

module Drool.UI.SamplingOptions (
  initComponent
) where

import Data.IORef

import qualified Graphics.UI.Gtk as Gtk
import qualified Graphics.UI.Gtk.Builder as GtkBuilder

import Graphics.Rendering.OpenGL

import qualified Drool.Utils.Conversions as Conv
import qualified Drool.Types as DT
import qualified Drool.Utils.SigGen as SigGen
import qualified Drool.ApplicationContext as AC (ContextSettings(..))

-- Initializes GUI component for view options.
-- Expects a GtkBuilder instance.
initComponent :: GtkBuilder.Builder -> IORef AC.ContextSettings -> IO Bool
initComponent gtkBuilder contextSettings = do
  putStrLn "Initializing Samplingoptions component"

  defaultSettings <- readIORef contextSettings

  let defaultSigGen = AC.signalGenerator defaultSettings

  adjSamplingFreq <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToAdjustment "adjSamplingFreq"
  Gtk.adjustmentSetValue adjSamplingFreq (fromIntegral $ AC.samplingFrequency defaultSettings)
  _ <- Gtk.onValueChanged adjSamplingFreq $ do
    val <- Gtk.adjustmentGetValue adjSamplingFreq
    settings <- readIORef contextSettings
    contextSettings $=! settings { AC.samplingFrequency = round val }

  adjRenderingFreq <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToAdjustment "adjRenderingFreq"
  Gtk.adjustmentSetValue adjRenderingFreq (fromIntegral $ AC.renderingFrequency defaultSettings)
  _ <- Gtk.onValueChanged adjRenderingFreq $ do
    val <- Gtk.adjustmentGetValue adjRenderingFreq
    settings <- readIORef contextSettings
    contextSettings $=! settings { AC.renderingFrequency = round val }

  adjBufferSize <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToAdjustment "adjBufferSize"
  Gtk.adjustmentSetValue adjBufferSize (fromIntegral $ AC.signalBufferSize defaultSettings)
  _ <- Gtk.onValueChanged adjBufferSize $ do
    val <- Gtk.adjustmentGetValue adjBufferSize
    settings <- readIORef contextSettings
    contextSettings $=! settings { AC.signalBufferSize = round val }

  adjBufferSamples <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToAdjustment "adjBufferSamples"
  Gtk.adjustmentSetValue adjBufferSamples (fromIntegral $ SigGen.numSamples defaultSigGen)
  _ <- Gtk.onValueChanged adjBufferSamples $ do
    val <- Gtk.adjustmentGetValue adjBufferSamples
    settings <- readIORef contextSettings
    let currentSigGen = AC.signalGenerator settings
    let siggen = currentSigGen { SigGen.numSamples = round val }
    contextSettings $=! settings { AC.signalGenerator = siggen }



  return True
