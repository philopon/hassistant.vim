{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Directory where

import qualified Hassistant.Directory.Internal as M

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Arbitrary

import qualified Filesystem.Path.CurrentOS as P

directory :: TestTree
directory = testGroup "Directory" [dirOrFileName]

dirOrFileName :: TestTree
dirOrFileName = testGroup "dirOrFileName"
    [ testProperty "on directory, == dirname" $
      \((P.</> "") . unAFP -> p) -> M.dirOrFileName p === P.dirname p
    , testProperty "on file, == filename" $
      \(unAFP -> p) -> not (P.null p) && last (P.encodeString p) /= '/' ==>
      M.dirOrFileName p === P.filename p
    ]

--moduleNameText = testGroup "moduleName-Text"
--    [ testProperty "moduleNameToPath "
