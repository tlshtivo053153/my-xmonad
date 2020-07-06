{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
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
, putCommandLineText
, toCompletion
, runCompletion
, runCompletion'
) where

import XMonad.Vim.Parse.Command ( Command, parse, parse', stringToEntry )
import XMonad.Vim.UI.Utils ( Position(..), OnWidget(..), Size(..), setPosition, resizeWindow, moveOnTop )
import XMonad.Vim.UI.Thread ( forkGUI, constForkGUI )

import Control.Concurrent ( forkIO, threadDelay )
import qualified Control.Concurrent.MVar as M

import qualified Data.GI.Gtk as Gtk
import Data.GI.Gtk.Threading ( postGUIASync )
import Data.GI.Base
import Data.GI.Base.ShortPrelude ( Int32 )
import qualified Data.GI.Base.GType as GT

import Control.Monad (liftM2, unless, forM_, when, join, (<=<) )
import qualified Data.Text as T

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
    , userInputText :: !(M.MVar T.Text)
    , parsedText :: !(M.MVar String)
    , isShowing :: !(M.MVar Bool)
    , mainWindow :: !Gtk.Window
    , completeWindow :: !Gtk.Window
    , completeEntry :: !Gtk.Entry
    , completeTree :: !Gtk.TreeView
    , completeListStore :: !Gtk.ListStore
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
    mainWindow_ <- new Gtk.Window [ #title := "XMonadVimCommandLineWindow"
                                  , #resizable := True
                                  ]
    completeWindow_ <- new Gtk.Window [ #type := Gtk.WindowTypePopup ]
    completeEntry_ <- new Gtk.Entry []
    completeTree_ <- new Gtk.TreeView []
    completeListStore_ <- new Gtk.ListStore []
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
                postGUIASync $ if b
                  then #showAll mainWindow'
                  else #hide mainWindow'
                waitShow
          waitCursorDir' <- asks waitCursorDir
          config <- ask
          let treeViewUpCursor' = runCompletion config treeViewUpCursor
              treeViewDownCursor' = runCompletion config treeViewDownCursor
          let waitCursor = do
                dir <- M.takeMVar waitCursorDir'
                postGUIASync $ case dir of
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
    #add mainWindow' completeEntry'
    let showSignalAction = do
          setPosition mainWindow' BottomLeft
          resizeWindow MaxWidth mainWindow' 
          #showAll completeWindow'
          #present completeWindow'
    let hideSignalAction = #hide completeWindow'
    toCompletion $ on mainWindow' #show =<< forkGUI showSignalAction
    toCompletion $ on mainWindow' #hide =<< forkGUI hideSignalAction
    return ()

initWindow :: Completion ()
initWindow = do
    isShowing' <- asks isShowing
    completeWindow' <- asks completeWindow
    completeTree' <- asks completeTree
    completeEntry' <- asks completeEntry
    treeSelection <- #getSelection completeTree'
    completeListStore <- asks completeListStore
    config <- ask

    #add completeWindow' completeTree'

    let showSignalAction = do
          isShow <- M.swapMVar isShowing' True
          threadDelay $ 1000 * 100 
          moveOnTop completeEntry' completeWindow'
          #setMode treeSelection Gtk.SelectionModeSingle
          -- #setText completeEntry' ""
          runCompletion config $ updateCompletion =<< completeFunction ""
          return ()
    let hideSignalAction = do
          isShow <- M.swapMVar isShowing' False
          #setMode treeSelection Gtk.SelectionModeNone
    let configureEventAction = do
          moveOnTop completeEntry' completeWindow'
          #present completeWindow'
    toCompletion $ on completeWindow' #show =<< forkGUI showSignalAction
    toCompletion $ on completeWindow' #hide =<< forkGUI hideSignalAction
    toCompletion $ on completeWindow' #configureEvent =<< constForkGUI False configureEventAction
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

    let editableChangedAction = do
          inputUser <- M.swapMVar isInputUser' True
          when inputUser $ do
            entryText <- #getText completeEntry'
            M.swapMVar userInputText' entryText
            update (T.unpack entryText)
    toCompletion $ on completeEntry' #changed =<< forkGUI editableChangedAction
    return ()

initTree :: Completion ()
initTree = do
    completeTree' <- asks completeTree
    completeListStore <- asks completeListStore

    config <- ask

    #setColumnTypes completeListStore [GT.gtypeString]
    #setModel completeTree' (Just completeListStore)
    #setHeadersVisible completeTree' False

    treeViewColumn <- new Gtk.TreeViewColumn []
    cellRendererText <- new Gtk.CellRendererText []
    #packStart treeViewColumn cellRendererText False
    #setCellDataFunc treeViewColumn cellRendererText $ Just $ \layout renderer model iter -> do
        text <- fromGValue =<< #getValue model iter 0 :: IO (Maybe T.Text)
        cell <- castTo Gtk.CellRendererText renderer
        case (text, cell) of
            (Just t, Just cell') -> set cell' [ #text := t ]
            _ -> return ()
    #appendColumn completeTree' treeViewColumn

    treeSelection <- #getSelection completeTree'
    #setMode treeSelection Gtk.SelectionModeSingle

    treeSelection <- #getSelection completeTree'
    let oneSelection' = runCompletion config oneSelection
    toCompletion $ on treeSelection #changed =<< forkGUI (join oneSelection')
    return ()

oneSelection :: Completion (IO ())
oneSelection = do
    completeEntry' <- asks completeEntry
    userInputText' <- asks userInputText
    isInputUser' <- asks isInputUser
    parsedText' <- asks parsedText
    text <- treeViewSelectedText

    return $ case text of
        (Just listText) -> do
          entryText <- Gtk.entryGetText completeEntry'
          when (listText /= entryText) $ do
            M.swapMVar isInputUser' False
            parsedText'' <- M.readMVar parsedText'
            Gtk.entrySetText completeEntry' $ if null parsedText''
                then T.pack $ stringToEntry $ T.unpack listText
                else T.pack $ unwords [parsedText'', stringToEntry $ T.unpack listText]
            Gtk.editableSetPosition completeEntry' (-1)
        Nothing -> do
          userInputText'' <- M.readMVar userInputText'
          entryText <- Gtk.entryGetText completeEntry'
          when (userInputText'' /= entryText) $ do
            M.swapMVar isInputUser' False
            Gtk.entrySetText completeEntry' userInputText''
            Gtk.editableSetPosition completeEntry' (-1)

data CursorDir = UpCursor | DownCursor

--treeViewCursorDir :: CursorDir -> Completion (IO ())
--treeViewCursorDir dir = do
--    completeTree' <- asks completeTree
--    treeSelection <- #getSelection completeTree'
--    (treePaths, _) <- treeViewRows
--    (placedIn, treeMin, treeMax) <- treeViewRange
--
--    --let showPosition = #getColumn completeTree' 0 >>= \(Just c) -> #cellGetPosition c (undefined :: Gtk.CellRenderer) >>= print
--    return $ case (dir, treePaths, treeMin, treeMax) of
--       ( UpCursor   , []     , _   , max)               -> putStrLn "up1" >> #setCursor completeTree' max Gtk.noTreeViewColumn False
--       --( UpCursor   , [path] , min , _) | min /= path   -> putStrLn "up2" >> #down path >> #setCursor completeTree' path Gtk.noTreeViewColumn False
--       ( UpCursor   , [path] , min , _) | min /= path   -> putStrLn "up2" >> #prev path >> #setCursor completeTree' path Gtk.noTreeViewColumn False
--       ( UpCursor   , _      , min , _)                 -> putStrLn "up3" >> #unselectPath treeSelection min
--       ( DownCursor , []     , min , _)                 -> putStrLn "down1" >> #setCursor completeTree' min Gtk.noTreeViewColumn False
--       --( DownCursor , [path] , _   , max) | max /= path -> putStrLn "down2" >> #up path >> #setCursor completeTree' path Gtk.noTreeViewColumn False
--       ( DownCursor , [path] , _   , max) | max /= path -> putStrLn "down2" >> #next path >> #setCursor completeTree' path Gtk.noTreeViewColumn False
--       ( DownCursor , _      , _   , max)               -> putStrLn "down3" >> #unselectPath treeSelection max

treeViewCursorDir :: CursorDir -> Completion (IO ())
treeViewCursorDir dir = do
    completeTree' <- asks completeTree
    treeSelection <- #getSelection completeTree'
    config <- ask
    return $ do
      (treePath, _) <- #getCursor completeTree'
      (placedIn, treeMin, treeMax) <- runCompletion config treeViewRange
      case (dir, treePath, treeMin, treeMax) of
         ( UpCursor, Nothing   , _   , max) -> #setCursor completeTree' max Gtk.noTreeViewColumn False
         ( UpCursor, Just path , min , max) -> do
            relative <- #compare path min
            isSelect <- #pathIsSelected treeSelection path
            case (relative, isSelect) of
                (_, False) -> #setCursor completeTree' max Gtk.noTreeViewColumn False
                (0, True)  -> #unselectPath treeSelection min
                _          -> #prev path >> #setCursor completeTree' path Gtk.noTreeViewColumn False
         ( DownCursor, Nothing  , min , _)   -> #setCursor completeTree' min Gtk.noTreeViewColumn False
         ( DownCursor, Just path, min , max) -> do
            relative <- #compare path max
            isSelect <- #pathIsSelected treeSelection path
            case (relative, isSelect) of
              (_, False) -> #setCursor completeTree' min Gtk.noTreeViewColumn False
              (0, True)  -> #unselectPath treeSelection max
              _          -> #next path >> #setCursor completeTree' path Gtk.noTreeViewColumn False

treeViewUpCursor :: Completion (IO ())
treeViewUpCursor = treeViewCursorDir UpCursor

treeViewDownCursor :: Completion (IO ())
treeViewDownCursor = treeViewCursorDir DownCursor

completeTreeGetSelected :: Completion (Bool, Gtk.TreeModel, Gtk.TreeIter)
completeTreeGetSelected =
      (#getSelected <=< #getSelection)
      =<< asks completeTree

treeViewSelectedText :: Completion (Maybe T.Text)
treeViewSelectedText = do
    --(isSelected, model, iter) <-
    --    toCompletion
    --    . (#getSelected <=< #getSelection)
    --    =<< asks completeTree
    (isSelected, model, iter) <- completeTreeGetSelected
    let r = if isSelected then Just (model, iter) else Nothing
    toCompletion $ if isSelected
        then fromGValue =<< #getValue model iter 0
        else return Nothing

treeViewRow :: Completion (Maybe Int32)
treeViewRow = do
--    (isSelected, model, iter) <-
--        toCompletion
--        . (#getSelected <=< #getSelection)
--        =<< asks completeTree
    (isSelected, _, iter) <- completeTreeGetSelected
    if isSelected
      then Just <$> get iter #stamp
      else return Nothing

treeViewRow' :: Completion (Maybe (Gtk.TreeModel, Gtk.TreeIter))
treeViewRow' = do
    (isSelected, model, iter) <- completeTreeGetSelected
    return $ if isSelected then Just (model, iter) else Nothing

treeViewRows :: Completion ([Gtk.TreePath], Gtk.TreeModel)
treeViewRows =
    (#getSelectedRows <=< #getSelection)
    =<< asks completeTree

treeViewRange :: Completion (Bool, Gtk.TreePath, Gtk.TreePath)
treeViewRange =
    #getVisibleRange =<< asks completeTree

updateCompletion :: [String] -> Completion ()
updateCompletion xs = do
    completeListStore' <- asks completeListStore
    toCompletion $ do
      #clear completeListStore'
      forM_ xs $ \str -> do
          iter <- #append completeListStore'
          #setValue completeListStore' iter 0 =<< toGValue (Just $ T.pack str)

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
    toCompletion . postGUIASync $ do
        M.swapMVar isInputUser' True
        #setText completeEntry' $ T.pack str
        #setPosition completeEntry' (-1)

putCommandLineText :: Completion ()
putCommandLineText = do
    completeEntry' <- asks completeEntry
    commandText' <- asks commandText
    toCompletion $ postGUIASync $ do
        entryText <- #getText completeEntry'
        M.putMVar commandText' (parse' $ T.unpack entryText)

toCompletion :: IO a -> Completion a
toCompletion = liftIO

runCompletion :: CompleteStatus -> Completion a -> IO a
runCompletion c (Completion completion) = runReaderT completion c

runCompletion' :: CompleteStatus -> Completion a -> IO a
runCompletion' c completion = runCompletion c (initCompletion >> completion)

