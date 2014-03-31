
{-# LANGUAGE OverloadedStrings #-}

module Arbitrary where

import Control.Monad

import qualified Data.Text as T
import Test.QuickCheck
import qualified Filesystem.Path.CurrentOS as P

newtype AFilePath = AFilePath { unAFP :: P.FilePath } deriving Show
instance Arbitrary AFilePath where
    arbitrary = sized $ \i -> do
        root <- arbitrary >>= \b -> return $ if b then "/" else ""
        strs <- replicateM i . listOf $ elements pathChar
        return . AFilePath . P.concat $ root : map P.decodeString strs
      where pathChar = "." ++ ['0' .. '9'] ++ ['a' .. 'z'] ++ ['A' .. 'Z']

newtype AModule = AModule { unAM :: T.Text } deriving Show
instance Arbitrary AModule where
    arbitrary = sized $ \i -> do
        a <- modElem
        b <- replicateM i modElem
        return . AModule . T.intercalate "." . map T.pack $ a : b
      where modChar = ['0' .. '9'] ++ ['a' .. 'z'] ++ ['A' .. 'Z']
            modElem = do
                h <- elements ['A' .. 'Z']
                o <- listOf $ elements modChar
                return $ h : o
