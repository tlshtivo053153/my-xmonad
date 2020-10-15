module MyConfig.Layout where

import XMonad

import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Hooks.ManageDocks ( AvoidStruts, avoidStruts )

import XMonad.Layout.Fullscreen ( fullscreenFull, FullscreenFull )

--myLayout :: ModifiedLayout AvoidStruts (Choose Tall (Choose (Mirror Tall) (ModifiedLayout FullscreenFull Full))) Window
myLayout :: Choose
    (ModifiedLayout AvoidStruts (Choose Tall (Mirror Tall)))
    (ModifiedLayout FullscreenFull (ModifiedLayout AvoidStruts Full))
    a
myLayout = avoidStruts (tiled ||| Mirror tiled ) ||| fullscreenFull (avoidStruts Full)
  where
    tiled = Tall nmaster delta ratio
    nmaster = 1
    ratio = 1/2
    delta = 3/100

