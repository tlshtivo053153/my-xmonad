{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module XMonad.Vim.UI.CommandLine
( Completion
, CompleteStatus(..)
, initCompletion
, newCompleteStatus
, showCommandLine
, hideCommandLine
, upCursor
, downCursor
, writeCommandLineText
, readCommandLineText
, putCommandLineText
, toCompletion
, runCompletion
, runCompletion'
) where

import XMonad.Vim.Parse.Command ( Command, parse, parse', stringToEntry )
import XMonad.Vim.UI.Utils ( Position(..), OnWidget(..), Size(..), fixPosition, resizeWindow, moveOnTop )
import XMonad.Vim.UI.Thread ( on', on'' )

import Control.Concurrent ( forkIO, threadDelay )
import qualified Control.Concurrent.MVar as M

import qualified Graphics.UI.Gtk as Gtk
import           Graphics.UI.Gtk ( AttrOp(..) )
import qualified System.Glib as Glib
import Control.Monad (liftM2, unless, forM_, when, join, (<=<) )

import Data.List ( isPrefixOf )
import Data.Maybe ( fromMaybe )

import Control.Monad.Trans (lift, liftIO)
import Control.Monad.Reader (ReaderT, MonadReader, runReaderT, asks, ask)
import Control.Monad.IO.Class (MonadIO)
import Data.Default

newtype Completion a = Completion (ReaderT CompleteStatus IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader CompleteStatus)

instance Default a => Default (Completion a) where
  def = return def

instance Semigroup a => Semigroup (Completion a) where
    (<>) = liftM2 (<>)

instance Monoid a => Monoid (Completion a) where
  mempty = return mempty
  mappend = liftM2 mappend

data CompleteStatus = CompleteStatus
    { isInitialized :: !(M.MVar Bool)
    , isInputUser :: !(M.MVar Bool)
    , userInputText :: !(M.MVar String)
    , parsedText :: !(M.MVar String)
    , isShowing :: !(M.MVar Bool)
    , mainWindow :: !Gtk.Window
    , completeWindow :: !Gtk.Window
    , completeEntry :: !Gtk.Entry
    , completeTree :: !Gtk.TreeView
    , completeListStore :: !(Gtk.ListStore String)
    , commandList :: !(M.MVar [Command])
    , commandText :: !(M.MVar [String])
    , waitShowing :: !(M.MVar Bool) -- True : show, False : hide
    , waitCursorDir :: !(M.MVar CursorDir)
    , waitText :: !(M.MVar String)
    }

newCompleteStatus :: IO CompleteStatus
newCompleteStatus = do
    isInitialized_ <- M.newMVar False
    isInputUser_ <- M.newMVar False
    userInputText_ <- M.newMVar ""
    parsedText_ <- M.newMVar ""
    isShowing_ <- M.newMVar False
    mainWindow_ <- Gtk.windowNew
    completeWindow_ <- Gtk.windowNewPopup
    completeEntry_ <- Gtk.entryNew
    completeTree_ <- Gtk.treeViewNew
    completeListStore_ <- Gtk.listStoreNew []
    commandList_ <- M.newMVar []
    commandText_ <- M.newEmptyMVar
    waitShowing_ <- M.newEmptyMVar
    waitCursorDir_ <- M.newEmptyMVar
    waitText_ <- M.newMVar ""
    return $ CompleteStatus
      { isInitialized    = isInitialized_
      , isInputUser      = isInputUser_
      , userInputText    = userInputText_
      , parsedText       = parsedText_
      , isShowing        = isShowing_
      , mainWindow       = mainWindow_
      , completeWindow   = completeWindow_
      , completeEntry    = completeEntry_
      , completeTree     = completeTree_
      , completeListStore = completeListStore_
      , commandList = commandList_
      , commandText = commandText_
      , waitShowing = waitShowing_
      , waitCursorDir = waitCursorDir_
      , waitText = waitText_
      }

initCompletion :: Completion ()
initCompletion = do
    isInitialized' <- toCompletion . flip M.swapMVar True =<< asks isInitialized
    unless isInitialized' $ do
          initMainWindow
          initWindow
          initEntry
          initTree
          waitShowing' <- asks waitShowing
          mainWindow' <- asks mainWindow
          let waitShow = do
                b <- M.takeMVar waitShowing'
                Gtk.postGUIAsync $ if b
                  then Gtk.widgetShowAll mainWindow'
                  else Gtk.widgetHideAll mainWindow'
                waitShow
          waitCursorDir' <- asks waitCursorDir
          config <- ask
          let treeViewUpCursor' = runCompletion config treeViewUpCursor
              treeViewDownCursor' = runCompletion config treeViewDownCursor
          let waitCursor = do
                dir <- M.takeMVar waitCursorDir'
                Gtk.postGUIAsync $ case dir of
                  UpCursor -> join treeViewUpCursor'
                  DownCursor -> join treeViewDownCursor'
                waitCursor
          toCompletion $ do
            forkIO waitShow
            forkIO waitCursor
          return ()

initMainWindow :: Completion ()
initMainWindow = do
    mainWindow' <- asks mainWindow
    completeEntry' <-  asks completeEntry
    completeWindow' <- asks completeWindow
    toCompletion $ Gtk.set mainWindow'
        [ Gtk.windowTitle := "XMonadVimCommandLineWindow"
        , Gtk.windowResizable := True
        , Gtk.windowAllowGrow := True
        ]
    toCompletion $ Gtk.containerAdd mainWindow' completeEntry'
    let showSignalAction = Gtk.postGUIAsync $ do
          fixPosition mainWindow' BottomLeft
          --Gtk.widgetShowAll completeWindow'
          resizeWindow MaxWidth mainWindow' 
          Gtk.widgetShowAll completeWindow'
          Gtk.windowPresent completeWindow'
        hideSignalAction = Gtk.postGUIAsync $ Gtk.widgetHideAll completeWindow'
    toCompletion $ on' mainWindow' Gtk.showSignal showSignalAction
    toCompletion $ on' mainWindow' Gtk.hideSignal hideSignalAction
    return ()

initWindow :: Completion ()
initWindow = do
    isShowing' <- asks isShowing
    completeWindow' <- asks completeWindow
    completeTree' <- asks completeTree
    completeEntry' <- asks completeEntry
    treeSelection <- toCompletion $ Gtk.treeViewGetSelection completeTree'

    toCompletion $ Gtk.containerAdd completeWindow' completeTree'

    let showSignalAction = do
          isShow <- M.swapMVar isShowing' True
          putStrLn "showSignalAction"
          Gtk.postGUIAsync $ do
            threadDelay $ 1000 * 100 
            moveOnTop completeEntry' completeWindow'
            Gtk.treeSelectionSetMode treeSelection Gtk.SelectionSingle
        hideSignalAction = do
          isShow <- M.swapMVar isShowing' False
          putStrLn "hideSignalAction"
          Gtk.postGUIAsync $
              Gtk.treeSelectionSetMode treeSelection Gtk.SelectionNone
        configureEventAction =
          Gtk.postGUIAsync $ do
              putStrLn "configreEventAction"
              moveOnTop completeEntry' completeWindow'
              Gtk.windowPresent completeWindow'
    toCompletion $ on' completeWindow' Gtk.showSignal showSignalAction
    toCompletion $ on' completeWindow' Gtk.hideSignal hideSignalAction
    toCompletion $ on'' False completeWindow' Gtk.configureEvent configureEventAction
    return ()

initEntry :: Completion ()
initEntry = do
    completeEntry' <- asks completeEntry
    completeWindow' <- asks completeWindow
    isInputUser' <- asks isInputUser
    userInputText' <- asks userInputText
    
    config <- ask
    let update str = runCompletion config (updateCompletion =<< completeFunction str)

    toCompletion $ update ""

    let editableChangedAction =
          Gtk.postGUIAsync $ do
            inputUser <- M.swapMVar isInputUser' True
            when inputUser $ do
              entryText <- Gtk.entryGetText completeEntry'
              M.swapMVar userInputText' entryText
              update entryText
    toCompletion $ Gtk.on completeEntry' Gtk.editableChanged editableChangedAction
    return ()

initTree :: Completion ()
initTree = do
    completeTree' <- asks completeTree
    completeListStore <- asks completeListStore

    config <- ask
    let oneSelection' = runCompletion config oneSelection

    toCompletion $ do
      Gtk.treeViewSetModel completeTree' (Just completeListStore)
      Gtk.treeViewSetHeadersVisible completeTree' False

      treeViewColumn <- Gtk.treeViewColumnNew
      cellRenderer <- Gtk.cellRendererTextNew
      Gtk.cellLayoutPackStart treeViewColumn cellRenderer False
      Gtk.cellLayoutSetAttributes treeViewColumn cellRenderer completeListStore $
        \row -> [Gtk.cellText := row]
      Gtk.treeViewAppendColumn completeTree' treeViewColumn
    toCompletion $ do
      treeSelection <- Gtk.treeViewGetSelection completeTree'
      Gtk.treeSelectionSetMode treeSelection Gtk.SelectionSingle

    treeSelection <- toCompletion $ Gtk.treeViewGetSelection completeTree'
    let treeSelectionSelectionChangedAction = Gtk.postGUIAsync $ join oneSelection'
    toCompletion $ on' treeSelection Gtk.treeSelectionSelectionChanged treeSelectionSelectionChangedAction
    return ()

oneSelection :: Completion (IO ())
oneSelection = do
    completeListStore' <- asks completeListStore
    completeEntry' <- asks completeEntry
    userInputText' <- asks userInputText
    isInputUser' <- asks isInputUser
    parsedText' <- asks parsedText
    row <- treeViewRow

    return $ case row of
        Just r -> do
          listText <- Gtk.listStoreGetValue completeListStore' r
          entryText <- Gtk.entryGetText completeEntry'
          when (listText /= entryText) $ do
            M.swapMVar isInputUser' False
            parsedText'' <- M.readMVar parsedText'
            Gtk.entrySetText completeEntry' $ if null parsedText''
                then stringToEntry listText
                else unwords [parsedText'', stringToEntry listText]
            Gtk.editableSetPosition completeEntry' (-1)
        Nothing -> do
          userInputText'' <- M.readMVar userInputText'
          entryText <- Gtk.entryGetText completeEntry'
          when (userInputText'' /= entryText) $ do
            M.swapMVar isInputUser' False
            Gtk.entrySetText completeEntry' userInputText''
            Gtk.editableSetPosition completeEntry' (-1)

data CursorDir = UpCursor | DownCursor

treeViewCursorDir :: CursorDir -> Completion (IO ())
treeViewCursorDir dir = do
    completeTree' <- asks completeTree
    treeSelection <- toCompletion $ Gtk.treeViewGetSelection completeTree'
    row <- treeViewRow
    (treeMin, treeMax) <- treeViewRange
    return $ case (dir, row, treeMin, treeMax) of
        (UpCursor, Nothing, _, [t]) -> Gtk.treeViewSetCursor completeTree' [t] Nothing
        (UpCursor, Just r, [t], _) | t <= r-1 -> Gtk.treeViewSetCursor completeTree' [r-1] Nothing
        (UpCursor, _, ts, _) -> Gtk.treeSelectionUnselectPath treeSelection ts
        (DownCursor, Nothing, [t], _) -> Gtk.treeViewSetCursor completeTree' [t] Nothing
        (DownCursor, Just r, _, [t]) | r+1 <= t -> Gtk.treeViewSetCursor completeTree' [r+1] Nothing
        (DownCursor, _, _, ts) -> Gtk.treeSelectionUnselectPath treeSelection ts

treeViewUpCursor :: Completion (IO ())
treeViewUpCursor = treeViewCursorDir UpCursor

treeViewDownCursor :: Completion (IO ())
treeViewDownCursor = treeViewCursorDir DownCursor

treeViewRow :: Completion (Maybe Int)
treeViewRow = do
    rows <-
        toCompletion
        . (Gtk.treeSelectionGetSelectedRows <=< Gtk.treeViewGetSelection)
        =<< asks completeTree
    return $ head' =<< head' rows

treeViewRange :: Completion (Gtk.TreePath, Gtk.TreePath)
treeViewRange = do
    completeTree' <- asks completeTree
    toCompletion $ Gtk.treeViewGetVisibleRange completeTree'

updateCompletion :: [String] -> Completion ()
updateCompletion xs = do
    completeListStore' <- asks completeListStore
    toCompletion $ do
      Gtk.listStoreClear completeListStore'
      forM_ xs $ Gtk.listStoreAppend completeListStore'

completeFunction :: String -> Completion [String]
completeFunction str = do
    commandList' <- asks commandList
    parsedText' <- asks parsedText
    toCompletion $ do
      commandList'' <- M.readMVar commandList'
      let (consumed, candidate) = parse commandList'' str
      M.swapMVar parsedText' (unwords $ map stringToEntry consumed)
      return candidate

showCommandLine :: Completion ()
showCommandLine = do
    waitShowing' <- asks waitShowing
    toCompletion $ M.putMVar waitShowing' True

hideCommandLine :: Completion ()
hideCommandLine = do
    waitShowing' <- asks waitShowing
    toCompletion $ M.putMVar waitShowing' False

upCursor :: Completion ()
upCursor = do
    waitCursorDir' <- asks waitCursorDir
    toCompletion $ M.putMVar waitCursorDir' UpCursor

downCursor :: Completion ()
downCursor = do
    waitCursorDir' <- asks waitCursorDir
    toCompletion $ M.putMVar waitCursorDir' DownCursor

writeCommandLineText :: String -> Completion ()
writeCommandLineText str = do
    completeEntry' <- asks completeEntry
    isInputUser' <- asks isInputUser
    toCompletion . Gtk.postGUIAsync $ do
        M.swapMVar isInputUser' True
        Gtk.entrySetText completeEntry' str
        Gtk.editableSetPosition completeEntry' (-1)

-- deprecated
readCommandLineText :: Completion ()
readCommandLineText = do
    completeEntry' <- asks completeEntry
    commandText' <- asks commandText
    toCompletion $ Gtk.postGUIAsync $ do
        entryText <- Gtk.entryGetText completeEntry'
        M.swapMVar commandText' (parse' entryText)
        return ()

putCommandLineText :: Completion ()
putCommandLineText = do
    completeEntry' <- asks completeEntry
    commandText' <- asks commandText
    toCompletion $ Gtk.postGUIAsync $ do
        entryText <- Gtk.entryGetText completeEntry'
        M.putMVar commandText' (parse' entryText)

toCompletion :: IO a -> Completion a
toCompletion = liftIO

runCompletion :: CompleteStatus -> Completion a -> IO a
runCompletion c (Completion completion) = runReaderT completion c

runCompletion' :: CompleteStatus -> Completion a -> IO a
runCompletion' c completion = runCompletion c (initCompletion >> completion)

head' :: [a] -> Maybe a
head' (x:_) = Just x
head' _     = Nothing

