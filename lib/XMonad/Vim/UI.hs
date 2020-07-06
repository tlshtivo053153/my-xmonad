module XMonad.Vim.UI
( showCommandLine
, hideCommandLine
, upCommandLineCursor
, downCommandLineCursor
, writeCommandLineText
, getCommandLineText
, writeStatusBarText
, updateStatusBarText
, showStatusBar
, hideStatusBar
, mainLoop
, initGUI
) where

import XMonad hiding (Position)

import XMonad.Vim.Core

import qualified XMonad.Vim.UI.StatusBar as UIS
import qualified XMonad.Vim.UI.CommandLine as UIC

import Control.Monad (when, unless, void)
import qualified Data.Text as T

import qualified GI.Gtk as Gtk

import qualified Data.IORef as R
import qualified Control.Concurrent.MVar as M

showCommandLine :: VimAction ()
showCommandLine = do
    cconfig <- asks commandLineConfig
    liftIO $ UIC.runCompletion cconfig UIC.showCommandLine

hideCommandLine :: VimAction ()
hideCommandLine = do
    cconfig <- asks commandLineConfig
    liftIO $ UIC.runCompletion cconfig UIC.hideCommandLine

upCommandLineCursor :: VimAction ()
upCommandLineCursor = do
    cconfig <- asks commandLineConfig
    liftIO $ UIC.runCompletion cconfig UIC.upCursor

downCommandLineCursor :: VimAction ()
downCommandLineCursor = do
    cconfig <- asks commandLineConfig
    liftIO $ UIC.runCompletion cconfig UIC.downCursor

writeCommandLineText :: String -> VimAction ()
writeCommandLineText str = do
    cconfig <- asks commandLineConfig
    liftIO . UIC.runCompletion cconfig $ UIC.writeCommandLineText str

getCommandLineText :: VimAction [String]
getCommandLineText = do
    cconfig <- asks commandLineConfig
    liftIO $ UIC.runCompletion cconfig $ do
        UIC.putCommandLineText
        UIC.toCompletion . M.takeMVar =<< asks UIC.commandText

showStatusBar :: VimAction ()
showStatusBar = do
    sconfig <- asks statusBarConfig
    liftIO $ UIS.runStatusBarAction sconfig UIS.showStatusBar

hideStatusBar :: VimAction ()
hideStatusBar = do
    sconfig <- asks statusBarConfig
    liftIO $ UIS.runStatusBarAction sconfig UIS.hideStatusBar

writeStatusBarText :: String -> VimAction ()
writeStatusBarText str = do
    sconfig <- asks statusBarConfig
    liftIO $ UIS.runStatusBarAction sconfig (UIS.writeStatusBarText str)

updateStatusBarText :: VimAction ()
updateStatusBarText = do
    sconfig <- asks statusBarConfig
    text <- gets statusBarText
    case text of
        (Just text') -> writeStatusBarText text'
        Nothing -> return ()

mainLoop :: IO ()
mainLoop = Gtk.main

initGUI :: IO ()
initGUI = void $ Gtk.init Nothing

main :: IO ()
main = return ()

