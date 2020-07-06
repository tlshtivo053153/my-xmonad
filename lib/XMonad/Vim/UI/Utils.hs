module XMonad.Vim.UI.Utils
( fixPosition
, setTopLeft
, moveOnTop
, moveOnBottom
, moveOn
, moveOnTBInScreen
, moveOnBTInScreen
, resizeWindow
, OnWidget(..)
, Position(..)
, Size(..)
) where

import XMonad hiding (Position, resizeWindow)

import Control.Monad (when, unless)

import Graphics.UI.Gtk ( AttrOp(..) )
import qualified Graphics.UI.Gtk as Gtk

data Position = TopLeft | TopRight | BottomLeft | BottomRight

fixPosition :: Gtk.Window -> Position -> IO ()
fixPosition win TopLeft     = setTopLeft win
fixPosition win TopRight    = setTopRight win
fixPosition win BottomLeft  = setBottomLeft win
fixPosition win BottomRight = setBottomRight win

setTopLeft :: Gtk.Window -> IO ()
setTopLeft win =
  Gtk.windowMove win 0 0

setTopRight :: Gtk.Window -> IO ()
setTopRight win = do
  sw <- Gtk.screenWidth
  (ww, _) <- Gtk.windowGetSize win
  Gtk.windowMove win (sw - ww) 0

setBottomLeft :: Gtk.Window -> IO ()
setBottomLeft win = do
  sh <- Gtk.screenHeight
  (_, wh) <- Gtk.windowGetSize win
  Gtk.windowMove win 0 (sh - wh)

setBottomRight :: Gtk.Window -> IO ()
setBottomRight win = do
  sw <- Gtk.screenWidth
  sh <- Gtk.screenHeight
  (ww, wh) <- Gtk.windowGetSize win
  Gtk.windowMove win (sw - ww) (sh - wh)

data OnWidget = OnTop | OnBottom | OnLeft | OnRight
    deriving (Eq)

moveOnTop :: (Gtk.WidgetClass widget) => widget -> Gtk.Window -> IO ()
moveOnTop = moveOn OnTop

moveOnBottom :: (Gtk.WidgetClass widget) => widget -> Gtk.Window -> IO ()
moveOnBottom = moveOn OnBottom

moveOn :: (Gtk.WidgetClass widget) => OnWidget -> widget -> Gtk.Window -> IO ()
moveOn position widget window = do
  drawWindow <- Gtk.widgetGetDrawWindow widget
  (drawX, drawY) <- Gtk.drawWindowGetOrigin drawWindow
  drawW <- Gtk.drawWindowGetWidth drawWindow
  drawH <- Gtk.drawWindowGetHeight drawWindow
  Gtk.Rectangle widX widY widW widH <- Gtk.widgetGetAllocation widget
  (_, winH) <- Gtk.windowGetSize window
  case position of
    p | p == OnTop || p == OnBottom ->
          Gtk.windowResize window widW . snd =<< Gtk.windowGetSize window
    _ -> flip (Gtk.windowResize window) widH . fst =<< Gtk.windowGetSize window
  uncurry (Gtk.windowMove window) $ case position of
    OnTop    -> (drawX + widX         , drawY + widY - winH)
    OnBottom -> (drawX + widX         , drawY + widY + widH)
    OnLeft   -> (drawX + widX - drawW , drawY + widY)
    OnRight  -> (drawX + widX + widW  , drawY + widY)

-- move on top or bottom, and in screen (default:top)
moveOnTBInScreen :: (Gtk.WidgetClass widget) => widget -> Gtk.Window -> IO ()
moveOnTBInScreen = moveOnInScreen OnTop

-- move on top or bottom, and in screen (default:bottom)
moveOnBTInScreen :: (Gtk.WidgetClass widget) => widget -> Gtk.Window -> IO ()
moveOnBTInScreen = moveOnInScreen OnBottom

moveOnInScreen :: (Gtk.WidgetClass widget) => OnWidget -> widget -> Gtk.Window -> IO ()
moveOnInScreen position widget window = do
  drawWindow <- Gtk.widgetGetDrawWindow widget
  (drawX, drawY) <- Gtk.drawWindowGetOrigin drawWindow
  drawW <- Gtk.drawWindowGetWidth drawWindow
  drawH <- Gtk.drawWindowGetHeight drawWindow
  (winW, winH) <- Gtk.windowGetSize window
  Gtk.Rectangle widX widY widW widH <- Gtk.widgetGetAllocation widget
  screenW <- Gtk.screenWidth
  screenH <- Gtk.screenHeight
  Gtk.windowResize window widW . snd =<< Gtk.windowGetSize window
  let (goTopX, goTopY) = (drawX + widX, drawY + widY - winH)
      (goBotX, goBotY) = (drawX + widX, drawY + widY + widH)
      goBotH = winH
  let (x, y) = case (goTopY > 0, goBotY + goBotH < screenH, position) of
        (True , _    , OnTop)    -> (goTopX, goTopY)
        (False, True , OnTop)    -> (goBotX, goBotY)
        (_    , True , OnBottom) -> (goBotX, goBotY)
        (True , False, OnBottom) -> (goTopX, goTopY)
        (False, False, _)
          | drawY + widY > screenH - (drawY + widY + widH) -> (goTopX, goTopY)
          | otherwise -> (goBotX, goBotY)
  Gtk.windowMove window x y

data Size = KeepSize
          | ToRight | ToLeft | ToUp | ToDown
          | MaxWidth | MaxHeight

resizeWindow :: Size -> Gtk.Window -> IO ()
resizeWindow size window = do
  (sw, sh) <- (,) <$> Gtk.screenWidth <*> Gtk.screenHeight
  (wx, wy) <- Gtk.windowGetPosition window
  (ww, wh) <- Gtk.windowGetSize window
  let (newW, newH) = case size of
                                    KeepSize  -> (ww      , wh      )
                                    ToRight   -> (sw - wx , wh      )
                                    ToLeft    -> (wx + ww , wh      )
                                    ToUp      -> (ww      , wy + wh )
                                    ToDown    -> (ww      , sh - wy )
                                    MaxWidth  -> (sw      , wh      )
                                    MaxHeight -> (ww      , sh      )
  Gtk.windowResize window newW newH

