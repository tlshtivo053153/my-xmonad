module XMonad.Vim.Action
( replicateAction
, countAction
, ignoreCount
, counting
, yankWindow
, yankWindowWS
, pasteWindow
, moveWindow
, kill1Window
, killAllOtherCopyWindows
, killSafeWindow
, changeMode
, callCommand
, runCommand
, cancelCommand
, registerAction
) where

import XMonad.Vim.Core
import qualified XMonad.Vim.UI as UI
import qualified XMonad.Vim.UI.CommandLine as UIC

import Control.Monad (join, forM, forM_, replicateM_, when )
import Control.Monad.State (modify, state)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map as M
import qualified Control.Concurrent.MVar as MVar

import XMonad hiding (moveWindow)
import qualified XMonad.StackSet as W
import XMonad.Actions.WindowBringer (windowMap')
import XMonad.Actions.CopyWindow (copyWindow, kill1, killAllOtherCopies)
import XMonad.Util.NamedWindows (getName)
import Numeric (showHex, readHex)
import qualified Graphics.X11 as X11

import qualified XMonad.Actions.CopyWindow.Alternative as CWA

replicateAction :: Int -> VimAction a -> VimAction ()
replicateAction = replicateM_

countAction :: (Int -> VimAction a) -> VimAction a
countAction f = join . state $ \s -> (f $ count s, s {count = 0})

ignoreCount :: VimAction a -> VimAction a
ignoreCount x = countAction $ const x

counting :: Int -> VimAction ()
counting n = modify $ \s -> s { count = count s * 10 + n}

decorateUniqueName :: WindowSpace -> Window -> X String
decorateUniqueName ws w = do
    name <- show <$> getName w
    return $  name ++ "[" ++ W.tag ws ++ "]" ++ showHex w ""

uniqueNameMap :: X (M.Map String Window)
uniqueNameMap = windowMap' decorateUniqueName

uniqueNameMap' :: X (M.Map String (Window,WindowSpace) )
uniqueNameMap' = undefined

readUniqueName :: String -> Maybe (WorkspaceId, Window)
readUniqueName uniName = (,) ws <$> win
  where
    ws = reverse . tail . takeWhile (/= '[') . dropWhile (/= ']' ) $ reverse uniName
    win = case readHex $ reverse $ takeWhile (/= ']') $ reverse uniName of
      [(w, _)] -> Just w
      _        -> Nothing

yankWindow :: Register -> VimAction ()
yankWindow reg = do
  ss <- xToVim $ gets windowset
  whenJust (W.peek ss) $ \w -> do
    let ws = W.workspace . W.current $ ss
    winName <- xToVim $ decorateUniqueName ws w
    modify $ \s -> s { register = M.insert reg winName (register s) }

yankWindowWS :: Register -> VimAction ()
yankWindowWS reg = do
  ss <- xToVim $ gets windowset
  winNames <- forM (W.index ss) $ \w -> do
    let ws = W.workspace . W.current $ ss
    xToVim $ decorateUniqueName ws w
  modify $ \s -> s { register = M.insert reg (unlines winNames) (register s) }

pasteWindow :: Register -> VimAction ()
pasteWindow reg = do
  ss <- xToVim $ gets windowset
  winNameMap <- gets register
  xToVim . whenJust (lines <$> M.lookup reg winNameMap) $ \winNames ->
    forM_ winNames $ \wn -> do
      winID <- M.lookup wn <$> uniqueNameMap
      whenJust winID $ \w ->
        windows $ copyWindow w (W.currentTag ss)

moveWindow :: Register -> VimAction ()
moveWindow reg = do
  ss <- xToVim $ gets windowset
  winNameMap <- gets register
  xToVim . whenJust (lines <$> M.lookup reg winNameMap) $ \winNames ->
    forM_ (map readUniqueName winNames) $ \wsWin ->
      whenJust wsWin $ \(ws, win) ->
        windows $ CWA.shiftWin ws (W.currentTag ss) win

kill1Window :: VimAction ()
kill1Window = xToVim kill1

kill1WindowWS :: VimAction ()
kill1WindowWS = undefined

killAllOtherCopyWindows :: VimAction ()
killAllOtherCopyWindows = xToVim killAllOtherCopies

killSafeWindow :: VimAction ()
killSafeWindow = xToVim $ do
    ss <- gets windowset
    whenJust (W.peek ss) $ \w -> when (W.member w $ delete'' w ss) $   windows $ delete'' w
    where delete'' w = W.modify Nothing (W.filter (/= w))

hintFocusWindow :: VimAction ()
hintFocusWindow = undefined

changeMode :: VimMode -> VimAction ()
changeMode after = do
  before <- gets mode
  modify $ \s -> s { mode = after }
  hook <- asks changeModeHook
  hook before after
  return ()

callCommand :: String -> VimAction ()
callCommand str = do
    (_, cs) <- join $ asks commandList
    cconfig <- asks commandLineConfig
    liftIO $ UIC.runCompletion cconfig $ do
        commandList' <- asks UIC.commandList
        UIC.toCompletion $ MVar.swapMVar commandList' cs
    UI.writeCommandLineText str
    changeMode Command

runCommand :: VimAction ()
runCommand = do
    (func, _) <- join $ asks commandList
    text <- UI.getCommandLineText
    changeMode Normal
    liftIO $ print text
    func text
    return ()

cancelCommand :: VimAction ()
cancelCommand = changeMode Normal

registerAction :: [Register] -> (Register -> VimAction ()) -> String -> String -> [ (String, VimAction ()) ]
registerAction rs action pre post = [ (pre ++ [r] ++ post, action r) | r <- rs ]

main :: IO ()
main = return ()

