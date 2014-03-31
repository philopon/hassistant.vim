{-# LANGUAGE ScopedTypeVariables #-}

module Hassistant.Imports where

import qualified GHC
import qualified Exception
import qualified Util
import qualified Outputable
import qualified GHC.Paths

import Control.Applicative

import qualified Data.Text as T
import qualified Data.Attoparsec.Text as A
import Data.Maybe(catMaybes)
import Data.List(sort)
import Data.Either (rights)
import Data.Int(Int32)

import Hassistant.Parser

imports :: T.Text -> [String]
imports = go [] . dropWhile (not . importLine).
          rights . map (A.parseOnly dropCommentP) . T.lines
  where 
    importLine ('i':'m':'p':'o':'r':'t':o) = null o || head o == ' '
    importLine _ = False
    go a []     = [unlines $ reverse a]
    go a (l:ls) | null l        = go a ls
                | ' ' == head l = go (l:a) ls
                | importLine l  = (unlines $ reverse a) : go [l] ls
                | otherwise     = [unlines $ reverse a]

parseImports :: String -> GHC.Ghc (Maybe (GHC.ImportDecl GHC.RdrName))
parseImports i = (Just <$> GHC.parseImportDecl i) `Exception.gcatch` handler
  where handler (_::Exception.SomeException) = return Nothing

importHash :: T.Text -> IO Int32
importHash cont = GHC.runGhc (Just GHC.Paths.libdir) $ do
    dyn <- GHC.getSessionDynFlags
    Util.hashString . unlines . sort . map (Outputable.showPpr dyn) . catMaybes <$> mapM parseImports (imports cont)

