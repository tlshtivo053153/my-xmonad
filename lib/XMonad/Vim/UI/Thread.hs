module XMonad.Vim.UI.Thread
( on'
, on''
) where

import Control.Monad ( void )
import Control.Monad.IO.Class ( MonadIO(..) )

import qualified Graphics.UI.Gtk as Gtk

import qualified Control.Concurrent.MVar as M
import Control.Concurrent ( forkIO )

on' :: MonadIO m => object -> Gtk.Signal object (m ()) -> IO a -> IO ()
on' = on'' ()

on'' :: MonadIO m => b -> object -> Gtk.Signal object (m b) -> IO a -> IO ()
on'' r object signal action = do
    runEvent <- M.newEmptyMVar
    endEvent <- M.newEmptyMVar
    let action' = do
          M.takeMVar runEvent
          Gtk.postGUIAsync $ void action
          M.putMVar endEvent ()
          action'
    Gtk.on object signal $ do
      liftIO $ do
        M.putMVar runEvent ()
        M.takeMVar endEvent
      return r
    forkIO action'
    return ()

