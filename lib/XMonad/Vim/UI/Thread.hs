module XMonad.Vim.UI.Thread
( forkGUI
, forkGUI'
, constForkGUI
) where

import Control.Monad ( void )

import Data.GI.Gtk.Threading ( postGUIASync )
import qualified Control.Concurrent.MVar as M
import Control.Concurrent ( forkIO, ThreadId )

--  HOWTO:
--     on object #signal =<< (forkGUI action)
forkGUI :: IO a -> IO (IO ())
forkGUI action = fst <$> forkGUI' action

forkGUI' :: IO a -> IO (IO (), ThreadId)
forkGUI' action = do
    (runEvent, endEvent) <- (,) <$> M.newEmptyMVar <*> M.newEmptyMVar
    let action' = do
          M.takeMVar runEvent
          postGUIASync $ void action
          M.putMVar endEvent ()
          action'
    let guiLoop = do
          M.putMVar runEvent ()
          M.takeMVar endEvent
    threadId <- forkIO action'
    return (guiLoop, threadId)

constForkGUI :: a -> IO b -> IO (c -> IO a)
constForkGUI r action = const . (>> return r) <$> forkGUI action

