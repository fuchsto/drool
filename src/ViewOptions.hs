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

module ViewOptions (
  initComponent
) where

import Graphics.UI.Gtk as Gtk
import Graphics.UI.Gtk.Builder as GtkBuilder

-- Initializes GUI component for view options.
-- Expects a GtkBuilder instance.
initComponent gtkBuilder = do
  putStrLn "Initializing Perspective component"

  button_view_perspectiveTop <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToButton "buttonPerspectiveTop"
  Gtk.onClicked button_view_perspectiveTop $ do
    putStrLn "Perspective: Top"

  button_view_perspectiveFront <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToButton "buttonPerspectiveFront"
  Gtk.onClicked button_view_perspectiveTop $ do
    putStrLn "Perspective: Front"

  button_view_perspectiveSide <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToButton "buttonPerspectiveSide"
  Gtk.onClicked button_view_perspectiveTop $ do
    putStrLn "Perspective: Side"

  button_view_perspectiveIso <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToButton "buttonPerspectiveIsometric"
  Gtk.onClicked button_view_perspectiveTop $ do
    putStrLn "Perspective: Isometric"



