-----------------------------------------------------------------------------
--
-- Module      :  ViewOptions
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

module Drool.UI.ViewOptions (
  initComponent
) where

import Data.IORef

import qualified Graphics.UI.Gtk as Gtk
import qualified Graphics.UI.Gtk.Builder as GtkBuilder

import Graphics.Rendering.OpenGL

import qualified Drool.Types as DT

-- Initializes GUI component for view options.
-- Expects a GtkBuilder instance.
initComponent :: GtkBuilder.Builder -> IORef DT.ContextSettings -> IO(Gtk.ConnectId Gtk.Adjustment)
initComponent gtkBuilder contextSettings = do
  putStrLn "Initializing Perspective component"

  button_view_perspectiveTop <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToButton "buttonPerspectiveTop"
  Gtk.onClicked button_view_perspectiveTop $ do
    settings <- readIORef contextSettings
    contextSettings $=! settings { DT.renderPerspective = DT.Top }

  button_view_perspectiveFront <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToButton "buttonPerspectiveFront"
  Gtk.onClicked button_view_perspectiveFront $ do
    settings <- readIORef contextSettings
    contextSettings $=! settings { DT.renderPerspective = DT.Front }

  button_view_perspectiveSide <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToButton "buttonPerspectiveSide"
  Gtk.onClicked button_view_perspectiveSide $ do
    settings <- readIORef contextSettings
    contextSettings $=! settings { DT.renderPerspective = DT.Side }

  button_view_perspectiveIso <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToButton "buttonPerspectiveIsometric"
  Gtk.onClicked button_view_perspectiveIso $ do
    settings <- readIORef contextSettings
    contextSettings $=! settings { DT.renderPerspective = DT.Isometric }

  -- scale_view_horScaling <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToHScale "hscaleHorizontalScaling"
  scale_view_horScalingAdj <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToAdjustment "adjHorizontalScaling"
  Gtk.onValueChanged scale_view_horScalingAdj $ do
    val <- Gtk.adjustmentGetValue scale_view_horScalingAdj
    settings <- readIORef contextSettings
    contextSettings $=! settings { DT.scaling = (realToFrac(val)::GLfloat) }

  -- scale_view_gridOpacity <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToHScale "hscaleGridOpacity"
  scale_view_gridOpacityAdj <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToAdjustment "adjGridOpacity"
  Gtk.onValueChanged scale_view_gridOpacityAdj $ do
    val <- Gtk.adjustmentGetValue scale_view_gridOpacityAdj
    settings <- readIORef contextSettings
    contextSettings $=! settings { DT.gridOpacity = (realToFrac(val)::GLfloat) }

  scale_view_surfaceOpacityAdj <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToAdjustment "adjSurfaceOpacity"
  Gtk.onValueChanged scale_view_surfaceOpacityAdj $ do
    val <- Gtk.adjustmentGetValue scale_view_surfaceOpacityAdj
    settings <- readIORef contextSettings
    contextSettings $=! settings { DT.surfaceOpacity = (realToFrac(val)::GLfloat) }


