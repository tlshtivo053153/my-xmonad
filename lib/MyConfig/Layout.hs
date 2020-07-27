module MyConfig.Layout where

import XMonad

import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Hooks.ManageDocks ( AvoidStruts, avoidStruts )

myLayout :: ModifiedLayout AvoidStruts (Choose Tall (Choose (Mirror Tall) Full)) a
myLayout = avoidStruts $ tiled ||| Mirror tiled ||| Full
  where
    tiled = Tall nmaster delta ratio
    nmaster = 1
    ratio = 1/2
    delta = 3/100

