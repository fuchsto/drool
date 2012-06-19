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
initComponent gtkBuilder contextSettings = do
  putStrLn "Initializing Perspective component"

  settings <- readIORef contextSettings

  button_view_perspectiveTop <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToButton "buttonPerspectiveTop"
  Gtk.onClicked button_view_perspectiveTop $ do
    putStrLn "Perspective: Top"
    contextSettings $=! settings { DT.renderPerspective = DT.Top }

  button_view_perspectiveFront <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToButton "buttonPerspectiveFront"
  Gtk.onClicked button_view_perspectiveFront $ do
    putStrLn "Perspective: Front"
    contextSettings $=! settings { DT.renderPerspective = DT.Front }

  button_view_perspectiveSide <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToButton "buttonPerspectiveSide"
  Gtk.onClicked button_view_perspectiveSide $ do
    putStrLn "Perspective: Side"
    contextSettings $=! settings { DT.renderPerspective = DT.Side }

  button_view_perspectiveIso <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToButton "buttonPerspectiveIsometric"
  Gtk.onClicked button_view_perspectiveIso $ do
    putStrLn "Perspective: Isometric"
    contextSettings $=! settings { DT.renderPerspective = DT.Isometric }

  scale_view_horScaling <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToHScale "hscaleHorizontalScaling"
  scale_view_horScalingAdj <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToAdjustment "adjHorizontalScaling"
  Gtk.onValueChanged scale_view_horScalingAdj $ do
    val <- Gtk.adjustmentGetValue scale_view_horScalingAdj
    settings <- readIORef contextSettings
    contextSettings $=! settings { DT.scaling = (realToFrac(val)::GLfloat) }


