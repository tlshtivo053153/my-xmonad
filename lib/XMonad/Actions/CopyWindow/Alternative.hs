module XMonad.Actions.CopyWindow.Alternative
( shift
, shiftWin
, kill1'
) where

import XMonad
import XMonad.StackSet (Stack, StackSet)
import qualified XMonad.StackSet as W

import Control.Monad (when)

import qualified XMonad.Actions.CopyWindow as CW

modify'' :: (Eq s, Eq i) => i -> Maybe (Stack a) -> (Stack a -> Maybe (Stack a)) ->
              StackSet i l a s sd -> StackSet i l a s sd
modify'' n d f s = W.view (W.currentTag s) . W.modify d f . W.view n $ s

delete'' :: (Eq s, Eq i, Eq a) => i -> a -> StackSet i l a s sd -> StackSet i l a s sd
delete'' n w = modify'' n Nothing (W.filter (/= w))

shift :: (Ord a, Eq s, Eq i) => i -> StackSet i l a s sd -> StackSet i l a s sd
shift n s = maybe s (\w -> shiftWin (W.currentTag s) n w s) (W.peek s)

shiftWin :: (Ord a, Eq s, Eq i) => i -> i -> a -> StackSet i l a s sd -> StackSet i l a s sd
shiftWin from to w s
  | from `W.tagMember` s && to `W.tagMember` s && from /= to
    && w `elem` W.index (W.view from s)
    = CW.copyWindow w to $ delete'' from w s
  | otherwise = s

kill1' :: WorkspaceId -> Window -> X ()
kill1' n w = do
  s <- gets windowset
  when (W.member w s) $
    if W.member w $ delete'' n w s
      then windows $ delete'' n w
      else killWindow w

