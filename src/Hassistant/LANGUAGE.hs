{-# LANGUAGE OverloadedStrings #-}

module Hassistant.LANGUAGE where

import qualified DynFlags

import qualified Data.Text as T
import Data.List

import Hassistant.Common

listLANGAUGE :: [Candidate]
listLANGAUGE =
    let obj s = (candidate $ T.pack s) { menu = Just "[LANGAUGE]" }
    in concatMap (\(s,_,_) -> [obj s, obj $ "No" ++ s]) DynFlags.xFlags

languageChar :: [Char]
languageChar = sort . nub $ concatMap (\(s,_,_) -> s) DynFlags.xFlags

