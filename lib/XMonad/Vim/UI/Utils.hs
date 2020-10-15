{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}

module XMonad.Vim.UI.Utils
( setPosition
, setTopLeft
, moveOnTop
, moveOnBottom
, moveOn
, resizeWindow
, OnWidget(..)
, Position(..)
, Size(..)
) 
where

import GHC.OverloadedLabels ( IsLabel )

import Control.Monad (when)

import Data.Text ()
import Data.Maybe ( isJust, fromJust )

import qualified Data.GI.Gtk as Gtk
import qualified GI.Gdk as Gdk
import GI.Gdk ( Rectangle )

--import Data.GI.Gtk ( IsLabel, Window )
import Data.GI.Base.ShortPrelude ( Int32 )

data Position = TopLeft | TopRight | BottomLeft | BottomRight

setPosition :: Gtk.Window -> Position -> IO ()
setPosition win TopLeft     = setTopLeft win
setPosition win TopRight    = setTopRight win
setPosition win BottomLeft  = setBottomLeft win
setPosition win BottomRight = setBottomRight win

getMonitorSize
    :: (IsLabel "getDisplay" (t -> IO Gdk.Display),
        IsLabel "getRootWindow" (t -> IO Gdk.Window)) =>
        t -> IO (Int32, Int32)
getMonitorSize win = do
  display <- #getDisplay win :: IO Gdk.Display
  monitor <- #getMonitorAtWindow display =<< (#getRootWindow win :: IO Gdk.Window) :: IO Gdk.Monitor
  rectangle <- #getWorkarea monitor
  width <- Gdk.get rectangle #width
  height <- Gdk.get rectangle #height
  return (width, height) :: IO (Int32, Int32)

setTopLeft :: Gtk.Window -> IO ()
setTopLeft win =
  #move win 0 0

setTopRight :: Gtk.Window -> IO ()
setTopRight win = do
  (monitorWidth, _) <- getMonitorSize win
  (windowWidth, _) <- #getSize win
  #move win (monitorWidth - windowWidth) 0

setBottomLeft :: Gtk.Window -> IO ()
setBottomLeft win = do
  sc <- #getScreen win
  sh <- #getHeight sc
  (_, wh) <- #getSize win
  #move win 0 (sh - wh)

setBottomRight :: Gtk.Window -> IO ()
setBottomRight win = do
  sc <- #getScreen win
  sw <- #getWidth sc
  sh <- #getHeight sc
  (ww, wh) <- #getSize win
  #move win (sw - ww) (sh - wh)

data OnWidget = OnTop | OnBottom | OnLeft | OnRight
    deriving (Eq)

moveOnTop ::
  (IsLabel "getAllocation" (t1 -> IO Rectangle),
   IsLabel "getParentWindow" (t1 -> IO (Maybe Gdk.Window)),
   IsLabel "getSize" (t2 -> IO (Int32, Int32)),
   IsLabel "move" (t2 -> Int32 -> Int32 -> IO ()),
   IsLabel "resize" (t2 -> Int32 -> Int32 -> IO ())) =>
  t1 -> t2 -> IO ()
moveOnTop = moveOn OnTop

moveOnBottom ::
  (IsLabel "getAllocation" (t1 -> IO Rectangle),
   IsLabel "getParentWindow" (t1 -> IO (Maybe Gdk.Window)),
   IsLabel "getSize" (t2 -> IO (Int32, Int32)),
   IsLabel "move" (t2 -> Int32 -> Int32 -> IO ()),
   IsLabel "resize" (t2 -> Int32 -> Int32 -> IO ())) =>
  t1 -> t2 -> IO ()
moveOnBottom = moveOn OnBottom

moveOn ::
  (IsLabel "getAllocation" (t1 -> IO Rectangle),
   IsLabel "getParentWindow" (t1 -> IO (Maybe Gdk.Window)),
   IsLabel "getSize" (t2 -> IO (Int32, Int32)),
   IsLabel "move" (t2 -> Int32 -> Int32 -> IO ()),
   IsLabel "resize" (t2 -> Int32 -> Int32 -> IO ())) =>
  OnWidget -> t1 -> t2 -> IO ()
moveOn position widget window = do
  parentWindow <- #getParentWindow widget :: IO (Maybe Gdk.Window)
  when (isJust parentWindow) $
    let rootWindow = fromJust parentWindow in do
          (rootX, rootY) <- #getPosition rootWindow
          --rootH <- #getHeight rootWindow
          rootW <- #getWidth rootWindow
          rec <- #getAllocation widget :: IO Gdk.Rectangle
          widX <- Gtk.get rec #x
          widY <- Gtk.get rec #y
          widW <- Gtk.get rec #width
          widH <- Gtk.get rec #height
          (winW, winH) <- #getSize window :: IO (Int32, Int32)
          let (baseX, baseY) = (rootX + widX, rootY + widY)
          () <- case position of
              p | p == OnTop || p == OnBottom -> #resize window widW winH
              _ -> #resize window winW widH
          let moveWin = uncurry (#move window) :: (Int32, Int32) -> IO ()
          moveWin $ case position of
              OnTop    -> (baseX         , baseY - winH)
              OnBottom -> (baseX         , baseY + widH)
              OnLeft   -> (baseX - rootW , baseY)
              OnRight  -> (baseX + widW  , baseY)

data Size = KeepSize
          | ToRight | ToLeft | ToUp | ToDown
          | MaxWidth | MaxHeight

resizeWindow :: Size -> Gtk.Window -> IO ()
resizeWindow size window = do
  (monitorW, monitorH) <- getMonitorSize window
  (wx, wy) <- #getPosition window
  (ww, wh) <- #getSize window
  let (newW, newH) = case size of
                                    KeepSize  -> (ww            , wh            )
                                    ToRight   -> (monitorW - wx , wh            )
                                    ToLeft    -> (wx + ww       , wh            )
                                    ToUp      -> (ww            , wy + wh       )
                                    ToDown    -> (ww            , monitorH - wy )
                                    MaxWidth  -> (monitorW      , wh            )
                                    MaxHeight -> (ww            , monitorH      )
  #resize window newW newH

