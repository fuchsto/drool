-----------------------------------------------------------------------------
--
-- Module      :  TransformationOptions
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

module Drool.UI.TransformationOptions (
  initComponent
) where

import Data.IORef

import Graphics.Rendering.OpenGL ( ($=!) )
import qualified Graphics.UI.Gtk as Gtk
import qualified Graphics.UI.Gtk.Builder as GtkBuilder

import qualified Drool.ApplicationContext as AC

-- Initializes GUI component for transformation options.
-- Expects a GtkBuilder instance and default context settings. 
initComponent :: GtkBuilder.Builder -> IORef AC.ContextSettings -> IORef AC.ContextObjects -> IO Bool
initComponent gtkBuilder contextSettings _ = do
  putStrLn "Initializing TransformationOptions component"

  -- defaultSettings <- readIORef contextSettings

  buttonSetNumFFTBands <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToButton "buttonSetNumFFTBands"
  _ <- Gtk.onClicked buttonSetNumFFTBands $ do 
    adjNumFFTBands <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToAdjustment "adjNumFFTBands"
    val <- Gtk.adjustmentGetValue adjNumFFTBands
    settings <- readIORef contextSettings
    contextSettings $=! settings { AC.numFFTBands = round val }

  return True
