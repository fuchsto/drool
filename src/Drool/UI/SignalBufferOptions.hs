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

module Drool.UI.SignalBufferOptions (
  initComponent, updateSettings
) where

import Data.IORef

import qualified Graphics.UI.Gtk as Gtk
import qualified Graphics.UI.Gtk.Builder as GtkBuilder

import Graphics.Rendering.OpenGL

import qualified Drool.Utils.SigGen as SigGen
import qualified Drool.ApplicationContext as AC ( ContextSettings(..), ContextObjects(..) )

-- Initializes GUI component for view options.
-- Expects a GtkBuilder instance.
initComponent :: GtkBuilder.Builder -> IORef AC.ContextSettings -> IORef AC.ContextObjects-> IO Bool
initComponent gtkBuilder contextSettings contextObjects = do
  putStrLn "Initializing SignalBufferOptions component"

  defaultSettings <- readIORef contextSettings
  defaultObjects  <- readIORef contextObjects

  let defaultSigGen = AC.signalGenerator defaultObjects

  adjSignalPushFreq <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToAdjustment "adjSignalPushFreq"
  _ <- Gtk.onValueChanged adjSignalPushFreq $ do
    val <- Gtk.adjustmentGetValue adjSignalPushFreq
    settings <- readIORef contextSettings
    contextSettings $=! settings { AC.signalPushFrequency = round val }

  adjRenderingFreq <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToAdjustment "adjRenderingFreq"
  _ <- Gtk.onValueChanged adjRenderingFreq $ do
    val <- Gtk.adjustmentGetValue adjRenderingFreq
    settings <- readIORef contextSettings
    contextSettings $=! settings { AC.renderingFrequency = round val }

  adjBufferSize <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToAdjustment "adjBufferSize"
  _ <- Gtk.onValueChanged adjBufferSize $ do
    val <- Gtk.adjustmentGetValue adjBufferSize
    settings <- readIORef contextSettings
    contextSettings $=! settings { AC.signalBufferSize = round val }

  adjBufferSamples <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToAdjustment "adjBufferSamples"
  Gtk.adjustmentSetValue adjBufferSamples (fromIntegral $ SigGen.numSamples defaultSigGen)
  _ <- Gtk.onValueChanged adjBufferSamples $ do
    val <- Gtk.adjustmentGetValue adjBufferSamples
    objects <- readIORef contextObjects
    let currentSigGen = AC.signalGenerator objects
    let siggen = currentSigGen { SigGen.numSamples = round val }
    contextObjects $=! objects { AC.signalGenerator = siggen }

  _ <- updateSettings gtkBuilder defaultSettings

  return True

updateSettings :: GtkBuilder.Builder -> AC.ContextSettings -> IO Bool
updateSettings gtkBuilder settings = do 
  adjSignalPushFreq <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToAdjustment "adjSignalPushFreq"
  Gtk.adjustmentSetValue adjSignalPushFreq (fromIntegral $ AC.signalPushFrequency settings)
  adjBufferSize <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToAdjustment "adjBufferSize"
  Gtk.adjustmentSetValue adjBufferSize (fromIntegral $ AC.signalBufferSize settings)
  adjRenderingFreq <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToAdjustment "adjRenderingFreq"
  Gtk.adjustmentSetValue adjRenderingFreq (fromIntegral $ AC.renderingFrequency settings)
  
  return True

