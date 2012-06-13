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

module Main where

import Debug.Trace

import qualified Graphics.UI.Gtk as Gtk
import qualified Graphics.UI.Gtk.Builder as GtkBuilder

import qualified ViewOptions
import qualified GLViewport

main = do
  Gtk.initGUI

  -- Load UI configuration from GtkBuilder file:
  builder <- GtkBuilder.builderNew
  GtkBuilder.builderAddFromFile builder "droolui-test.glade"

  -- Instatiate window from GtkBuilder file:
  mainWindow <- GtkBuilder.builderGetObject builder Gtk.castToWindow "mainWindow"

  -- Application exit callback (quits main loop):
  Gtk.onDestroy mainWindow Gtk.mainQuit

  -- Define button callbacks:
{-
  menuItem_FileClose <- GtkBuilder.builderGetObject builder Gtk.castToMenuItem "menuitemFileQuit"
  Gtk.on menuItem_FileClose menuItemActivate $ do
    putStrLn "Exiting"
    widgetDestroy mainWindow
-}

  ViewOptions.initComponent builder
  GLViewport.initComponent builder

  -- Display window:
  Gtk.widgetShowAll mainWindow

  -- Enter GUI main loop:
  Gtk.mainGUI
