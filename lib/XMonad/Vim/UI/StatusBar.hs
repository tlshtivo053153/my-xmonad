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

import XMonad.Vim.UI.Utils ( Position(..), fixPosition )
import XMonad.Vim.UI.Thread ( on', on'' )

import qualified Control.Concurrent.MVar as M

import qualified Graphics.UI.Gtk as Gtk
import           Graphics.UI.Gtk ( AttrOp(..) )

import Control.Monad ( liftM2, unless, forever, (<=<) )
import Control.Monad.Trans (lift, liftIO)
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
    statusBarWindow_ <- Gtk.windowNewPopup
    statusBarLabel_ <- Gtk.labelNew (Nothing :: Maybe String)
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

    toStatusBarAction $ Gtk.containerAdd statusBarWindow' statusBarLabel'

    let showSignalAction = Gtk.postGUIAsync $ fixPosition statusBarWindow' BottomRight
    let configureEventAction = Gtk.postGUIAsync $ fixPosition statusBarWindow' BottomRight
    toStatusBarAction $ on' statusBarWindow' Gtk.showSignal showSignalAction
    toStatusBarAction $ on'' False statusBarWindow' Gtk.configureEvent configureEventAction
    waitShowing' <- asks waitShowing
    let waitShow = do
          b <- M.takeMVar waitShowing'
          Gtk.postGUIAsync $ if b
            then Gtk.widgetShowAll statusBarWindow'
            else Gtk.widgetHideAll statusBarWindow'
          waitShow
    toStatusBarAction $ forkIO waitShow
    return ()

updateLabel :: StatusBarAction ()
updateLabel = do
    text' <- toStatusBarAction . M.takeMVar =<< asks text
    wrapText' <- asks wrapText
    statusBarLabel' <- asks statusBarLabel
    toStatusBarAction . Gtk.postGUIAsync $ Gtk.labelSetMarkup statusBarLabel' (wrapText' text')
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

