module MyConfig.ManageHook where

import XMonad
import qualified XMonad.StackSet as W

myManageHook :: ManageHook
myManageHook = composeAll . concat $
    [ [className =? c       --> unfloat | c <- myNoFloatsC ]
    , [title =? "XMonadVimCommandLineWindow" --> doFloat ]
    ]
    where
        unfloat     = ask >>= doF . W.sink
        myNoFloatsC = ["Xephyr"]

