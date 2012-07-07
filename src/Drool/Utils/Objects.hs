-----------------------------------------------------------------------------
--
-- Module      :  Drool.UI.GLWindow
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

{-# OPTIONS -O2 -Wall #-}

module Drool.Utils.Objects (
    renderCube, 
    renderCubeFrame
) where

import Graphics.Rendering.OpenGL as GL (GLfloat, Vertex3(..), PrimitiveMode(..), renderPrimitive, vertex )

vertify3 :: [(GLfloat,GLfloat,GLfloat)] -> IO ()
vertify3 verts = sequence_ $ map (\(a,b,c) -> vertex $ Vertex3 a b c) verts 
 
renderCube w = renderPrimitive Quads $ vertify3
      [ ( w, w, w), ( w, w,-w), ( w,-w,-w), ( w,-w, w),
        ( w, w, w), ( w, w,-w), (-w, w,-w), (-w, w, w),
        ( w, w, w), ( w,-w, w), (-w,-w, w), (-w, w, w),
        (-w, w, w), (-w, w,-w), (-w,-w,-w), (-w,-w, w),
        ( w,-w, w), ( w,-w,-w), (-w,-w,-w), (-w,-w, w),
        ( w, w,-w), ( w,-w,-w), (-w,-w,-w), (-w, w,-w) ]


renderCubeFrame w = renderPrimitive Lines $ vertify3
  [ ( w,-w, w), ( w, w, w),  ( w, w, w), (-w, w, w),
    (-w, w, w), (-w,-w, w),  (-w,-w, w), ( w,-w, w),
    ( w,-w, w), ( w,-w,-w),  ( w, w, w), ( w, w,-w),
    (-w, w, w), (-w, w,-w),  (-w,-w, w), (-w,-w,-w),
    ( w,-w,-w), ( w, w,-w),  ( w, w,-w), (-w, w,-w),
    (-w, w,-w), (-w,-w,-w),  (-w,-w,-w), ( w,-w,-w) ]


