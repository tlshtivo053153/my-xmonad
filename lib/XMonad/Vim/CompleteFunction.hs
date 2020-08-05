module XMonad.Vim.CompleteFunction
( isInfixOf'
, isPrefixOf'
, isSuffixOf'
, isIgnoreCase
) where

import qualified Data.List as L
import qualified Data.Char as C

isInfixOf' :: String -> String -> Bool
isInfixOf' = isIgnoreCase L.isInfixOf

isPrefixOf' :: String -> String -> Bool
isPrefixOf' = isIgnoreCase L.isPrefixOf

isSuffixOf' :: String -> String -> Bool
isSuffixOf' = isIgnoreCase L.isSuffixOf

isIgnoreCase :: (String -> String -> Bool) -> String -> String -> Bool
isIgnoreCase f xs ys = f (map C.toLower xs) (map C.toLower ys)
