-----------------------------------------------------------------------------
--
-- Module      :  Drool.Utils.Conversions
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Drool.Utils.Conversions (
    gtkColorToGLColor,
    glColorToGtkColor,
    freqToMs,
    msToFreq
) where

import qualified Graphics.UI.Gtk as Gtk
import Graphics.Rendering.OpenGL


gtkColorToGLColor :: Gtk.Color -> Color3 GLfloat
gtkColorToGLColor (Gtk.Color r g b) = Color3 r' g' b'
  where r' = ((fromIntegral r) / 65535.0) :: GLfloat
        g' = ((fromIntegral g) / 65535.0) :: GLfloat
        b' = ((fromIntegral b) / 65535.0) :: GLfloat

glColorToGtkColor :: Color3 GLfloat -> Gtk.Color
glColorToGtkColor (Color3 r g b) = Gtk.Color r' g' b'
  where r' = round(r * 65535.0)
        g' = round(g * 65535.0)
        b' = round(b * 65535.0)


freqToMs :: Int -> Int
freqToMs f = round(1000.0 / fromIntegral f)

msToFreq :: Int -> Int
msToFreq ms = round(1000.0 / fromIntegral ms)
