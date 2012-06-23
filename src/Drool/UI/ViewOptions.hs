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
import qualified Drool.ApplicationContext as AC

-- Initializes GUI component for view options.
-- Expects a GtkBuilder instance.
initComponent :: GtkBuilder.Builder -> IORef AC.ContextSettings -> IO Bool
initComponent gtkBuilder contextSettings = do
  putStrLn "Initializing Perspective component"

  defaultSettings <- readIORef contextSettings

  button_view_perspectiveTop <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToButton "buttonPerspectiveTop"
  _ <- Gtk.onClicked button_view_perspectiveTop $ do
    settings <- readIORef contextSettings
    contextSettings $=! settings { AC.renderPerspective = DT.Top }

  button_view_perspectiveFront <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToButton "buttonPerspectiveFront"
  _ <- Gtk.onClicked button_view_perspectiveFront $ do
    settings <- readIORef contextSettings
    contextSettings $=! settings { AC.renderPerspective = DT.Front }

  button_view_perspectiveSide <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToButton "buttonPerspectiveSide"
  _ <- Gtk.onClicked button_view_perspectiveSide $ do
    settings <- readIORef contextSettings
    contextSettings $=! settings { AC.renderPerspective = DT.Side }

  button_view_perspectiveIso <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToButton "buttonPerspectiveIsometric"
  _ <- Gtk.onClicked button_view_perspectiveIso $ do
    settings <- readIORef contextSettings
    contextSettings $=! settings { AC.renderPerspective = DT.Isometric }

  scale_view_horScalingAdj <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToAdjustment "adjHorizontalScaling"
  Gtk.adjustmentSetValue scale_view_horScalingAdj (realToFrac $ AC.scaling defaultSettings)
  _ <- Gtk.onValueChanged scale_view_horScalingAdj $ do
    val <- Gtk.adjustmentGetValue scale_view_horScalingAdj
    settings <- readIORef contextSettings
    contextSettings $=! settings { AC.scaling = (realToFrac(val)::GLfloat) }

  scale_view_gridOpacityAdj <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToAdjustment "adjGridOpacity"
  Gtk.adjustmentSetValue scale_view_gridOpacityAdj (realToFrac $ AC.gridOpacity defaultSettings)
  _ <- Gtk.onValueChanged scale_view_gridOpacityAdj $ do
    val <- Gtk.adjustmentGetValue scale_view_gridOpacityAdj
    settings <- readIORef contextSettings
    contextSettings $=! settings { AC.gridOpacity = (realToFrac(val)::GLfloat) }

  scale_view_surfaceOpacityAdj <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToAdjustment "adjSurfaceOpacity"
  Gtk.adjustmentSetValue scale_view_surfaceOpacityAdj (realToFrac $ AC.surfaceOpacity defaultSettings)
  _ <- Gtk.onValueChanged scale_view_surfaceOpacityAdj $ do
    val <- Gtk.adjustmentGetValue scale_view_surfaceOpacityAdj
    settings <- readIORef contextSettings
    contextSettings $=! settings { AC.surfaceOpacity = (realToFrac(val)::GLfloat) }

  colorbuttonGrid <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToColorButton "colorbuttonGrid"
  Gtk.colorButtonSetColor colorbuttonGrid (DT.glColorToGtkColor $ AC.gridColor defaultSettings)
  _ <- Gtk.onColorSet colorbuttonGrid $ do
    gtkColor <- Gtk.colorButtonGetColor colorbuttonGrid
    let val = DT.gtkColorToGLColor(gtkColor)
    settings <- readIORef contextSettings
    contextSettings $=! settings { AC.gridColor = val }

  colorbuttonSurface <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToColorButton "colorbuttonSurface"
  Gtk.colorButtonSetColor colorbuttonSurface (DT.glColorToGtkColor $ AC.surfaceColor defaultSettings)
  _ <- Gtk.onColorSet colorbuttonSurface $ do
    gtkColor <- Gtk.colorButtonGetColor colorbuttonSurface
    let val = DT.gtkColorToGLColor(gtkColor)
    settings <- readIORef contextSettings
    contextSettings $=! settings { AC.surfaceColor = val }

  colorbuttonLight <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToColorButton "colorbuttonLight"
  Gtk.colorButtonSetColor colorbuttonLight (DT.glColorToGtkColor $ AC.lightColor defaultSettings)
  _ <- Gtk.onColorSet colorbuttonLight $ do
    gtkColor <- Gtk.colorButtonGetColor colorbuttonLight
    let val = DT.gtkColorToGLColor(gtkColor)
    settings <- readIORef contextSettings
    contextSettings $=! settings { AC.lightColor = val }

  return True


