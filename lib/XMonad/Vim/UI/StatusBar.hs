{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module XMonad.Vim.UI.StatusBar
( StatusBarConfig(..)
, StatusBarAction
, newStatusBarConfig
, initStatusBarAction
, showStatusBar
, hideStatusBar
, writeStatusBarText
, runStatusBarAction
, runStatusBarAction'
)
where

import XMonad.Vim.UI.Utils ( Position(..), setPosition )
import XMonad.Vim.UI.Thread ( forkGUI, constForkGUI )
--import UI.Thread ( on', on'' )

import qualified Control.Concurrent.MVar as M

--import qualified Graphics.UI.Gtk as Gtk
--import           Graphics.UI.Gtk ( AttrOp(..) )

import qualified Data.GI.Gtk as Gtk
import qualified GI.Gdk as Gdk
import Data.GI.Gtk.Threading ( postGUIASync )
import Data.GI.Base

import qualified Data.Text as T

import Control.Monad ( liftM2, unless )
import Control.Monad.Trans (liftIO)
import Control.Monad.Reader (ReaderT, MonadReader, runReaderT, asks, ask)
import Control.Monad.IO.Class (MonadIO)
import Control.Concurrent (forkIO)

newtype StatusBarAction a = StatusBarAction (ReaderT StatusBarConfig IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader StatusBarConfig)

instance Semigroup a => Semigroup (StatusBarAction a) where
    (<>) = liftM2 (<>)

instance Monoid a => Monoid (StatusBarAction a) where
  mempty = return mempty
  mappend = liftM2 mappend

data StatusBarConfig = StatusBarConfig
    { isInitialized :: !(M.MVar Bool)
    , statusBarWindow :: !Gtk.Window
    , statusBarLabel :: !Gtk.Label
    , text :: !(M.MVar String)
    , wrapText :: !(String -> String)
    , waitShowing :: !(M.MVar Bool) -- True : show, False : hide
    }

newStatusBarConfig :: IO StatusBarConfig
newStatusBarConfig = do
    isInitialized_ <- M.newMVar False
    statusBarWindow_ <- new Gtk.Window [ #type := Gtk.WindowTypePopup ]
    statusBarLabel_ <- new Gtk.Label [ #label := "" ]
    text_ <- M.newMVar ""
    let wrapText_ str = "<big>" ++ str ++ "</big>"
    waitShowing_ <- M.newEmptyMVar
    return $ StatusBarConfig
      { isInitialized = isInitialized_
      , statusBarWindow = statusBarWindow_
      , statusBarLabel = statusBarLabel_
      , text = text_
      , wrapText = wrapText_
      , waitShowing = waitShowing_
      }

initStatusBarAction :: StatusBarAction ()
initStatusBarAction = do
    isInitialized' <- toStatusBarAction . flip M.swapMVar True =<< asks isInitialized
    config <- ask
    let updateLabelIO = runStatusBarAction config updateLabel
    unless isInitialized' $ do
        toStatusBarAction . forkIO $ updateLabelIO
        initStatusBarWindow

initStatusBarWindow :: StatusBarAction ()
initStatusBarWindow = do
    statusBarWindow' <- asks statusBarWindow
    statusBarLabel' <- asks statusBarLabel
    waitShowing' <- asks waitShowing

    toStatusBarAction $ do
      #add statusBarWindow' statusBarLabel'

      let showSignalAction = setPosition statusBarWindow' BottomRight
      let configureEventAction = setPosition statusBarWindow' BottomRight
      do
        action <- forkGUI showSignalAction
        Gdk.on statusBarWindow' #show action
      do
        action <- constForkGUI False configureEventAction
        Gdk.on statusBarWindow' #configureEvent action
      let waitShow = do
            b <- M.takeMVar waitShowing'
            postGUIASync $ if b
              then #showAll statusBarWindow'
              else #hide statusBarWindow'
            waitShow
      forkIO waitShow
      return ()

updateLabel :: StatusBarAction ()
updateLabel = do
    text' <- toStatusBarAction . M.takeMVar =<< asks text
    wrapText' <- asks wrapText
    statusBarLabel' <- asks statusBarLabel
    toStatusBarAction . postGUIASync $ #setMarkup statusBarLabel' (T.pack $ wrapText' text')
    updateLabel

showStatusBar :: StatusBarAction ()
showStatusBar = do
    waitShowing' <- asks waitShowing
    toStatusBarAction $ M.putMVar waitShowing' True

hideStatusBar :: StatusBarAction ()
hideStatusBar = do
    waitShowing' <- asks waitShowing
    toStatusBarAction $ M.putMVar waitShowing' False

writeStatusBarText :: String -> StatusBarAction ()
writeStatusBarText str = do
    text' <- asks text
    toStatusBarAction $ M.putMVar text' str

toStatusBarAction :: IO a -> StatusBarAction a
toStatusBarAction = liftIO

runStatusBarAction :: StatusBarConfig -> StatusBarAction a -> IO a
runStatusBarAction c (StatusBarAction action) = runReaderT action c

runStatusBarAction' :: StatusBarConfig -> StatusBarAction a -> IO a
runStatusBarAction' c action = runStatusBarAction c (initStatusBarAction >> action)

