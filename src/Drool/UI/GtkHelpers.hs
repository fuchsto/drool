-----------------------------------------------------------------------------
--
-- Module      :  GtkHelpers
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  Tobias Fuchs
-- Stability   :  experimental
-- Portability :  Win32, POSIX
--
-- |
--
-----------------------------------------------------------------------------

{-# OPTIONS -O2 -Wall #-}

module Drool.UI.GtkHelpers (
  bindAdjustment, 
  initAdjustment, 

  bindButton, 

  bindCheckButton,
  initCheckButton, 

  bindColorButton,
  initColorButton, 

  bindColorAlphaButton,
  initColorAlphaButton
) where

import Data.IORef

import qualified Graphics.Rendering.OpenGL as GL ( GLfloat, Color4 )

import qualified Graphics.UI.Gtk as Gtk
import qualified Graphics.UI.Gtk.Builder as GtkBuilder

import qualified Drool.ApplicationContext as AC
import qualified Drool.Utils.Conversions as Conv ( gtkColorToGLColor4, glColor4ToGtkColor )

bindAdjustment :: [Char] -> GtkBuilder.Builder -> IORef AC.ContextSettings -> (Double -> AC.ContextSettings -> AC.ContextSettings) -> IO ()
bindAdjustment elemId gtkBuilder contextSettings updateFun = do
  adj <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToAdjustment elemId
  _ <- Gtk.onValueChanged adj $ do
    val <- Gtk.adjustmentGetValue adj
    modifyIORef contextSettings (updateFun val) 
  return ()

initAdjustment :: [Char] -> GtkBuilder.Builder -> Double -> IO ()
initAdjustment elemId gtkBuilder value = do
  adj <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToAdjustment elemId
  Gtk.adjustmentSetValue adj value
  
bindButton :: [Char] -> GtkBuilder.Builder -> IORef AC.ContextSettings -> (AC.ContextSettings -> IO ()) -> IO ()
bindButton elemId gtkBuilder contextSettings command = do
  button <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToButton elemId
  _ <- Gtk.onClicked button $ do 
    settings <- readIORef contextSettings
    command settings
  return ()

bindCheckButton :: [Char] -> GtkBuilder.Builder -> IORef AC.ContextSettings -> (Bool -> AC.ContextSettings -> AC.ContextSettings) -> IO ()
bindCheckButton elemId gtkBuilder contextSettings updateFun = do 
  checkbutton <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToCheckButton elemId
  _ <- Gtk.on checkbutton Gtk.toggled $ do 
    val <- Gtk.toggleButtonGetActive checkbutton
    modifyIORef contextSettings (updateFun val)
  return ()

initCheckButton :: [Char] -> GtkBuilder.Builder -> Bool -> IO ()
initCheckButton elemId gtkBuilder value = do
  checkbutton <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToCheckButton elemId
  Gtk.toggleButtonSetActive checkbutton value

bindColorAlphaButton :: [Char] -> GtkBuilder.Builder -> IORef AC.ContextSettings -> (GL.Color4 GL.GLfloat -> AC.ContextSettings -> AC.ContextSettings) -> IO ()
bindColorAlphaButton elemId gtkBuilder contextSettings updateFun = do
  colorbutton <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToColorButton elemId
  Gtk.colorButtonSetUseAlpha colorbutton True
  _ <- Gtk.onColorSet colorbutton $ do
    gtkColor     <- Gtk.colorButtonGetColor colorbutton
    gtkAlpha     <- Gtk.colorButtonGetAlpha colorbutton
    let val       = Conv.gtkColorToGLColor4 gtkColor gtkAlpha
    modifyIORef contextSettings (updateFun val)
  return ()

bindColorButton :: [Char] -> GtkBuilder.Builder -> IORef AC.ContextSettings -> (GL.Color4 GL.GLfloat -> AC.ContextSettings -> AC.ContextSettings) -> IO ()
bindColorButton elemId gtkBuilder contextSettings updateFun = do
  colorbutton <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToColorButton elemId
  Gtk.colorButtonSetUseAlpha colorbutton False
  _ <- Gtk.onColorSet colorbutton $ do
    gtkColor <- Gtk.colorButtonGetColor colorbutton
    gtkAlpha <- Gtk.colorButtonGetAlpha colorbutton
    let val   = Conv.gtkColorToGLColor4 gtkColor gtkAlpha
    modifyIORef contextSettings (updateFun val)
  return ()

initColorAlphaButton :: [Char] -> GtkBuilder.Builder -> GL.Color4 GL.GLfloat -> IO ()
initColorAlphaButton elemId gtkBuilder value = do
  colorbutton <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToColorButton elemId
  Gtk.colorButtonSetUseAlpha colorbutton True
  let gtkColor = Conv.glColor4ToGtkColor $ value
  Gtk.colorButtonSetColor colorbutton (fst gtkColor)
  Gtk.colorButtonSetAlpha colorbutton (snd gtkColor)

initColorButton :: [Char] -> GtkBuilder.Builder -> GL.Color4 GL.GLfloat -> IO ()
initColorButton elemId gtkBuilder value = do
  colorbutton <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToColorButton elemId
  Gtk.colorButtonSetUseAlpha colorbutton True
  let gtkColor = Conv.glColor4ToGtkColor $ value
  Gtk.colorButtonSetColor colorbutton (fst gtkColor)
  Gtk.colorButtonSetAlpha colorbutton (snd gtkColor)


