
module Hassistant.Common where

import qualified GHC
import qualified Outputable
import qualified Pretty

import Control.Applicative

import Foreign.C

import qualified Data.ByteString.Unsafe as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

bool :: a -> a -> Bool -> a
bool t _ True  = t
bool _ f False = f
{-# INLINE bool #-}

unsafePackCStringToText :: CString -> IO T.Text
unsafePackCStringToText cstr = do
    T.decodeUtf8 <$> S.unsafePackCString cstr

showSDoc :: Outputable.PrintUnqualified -> GHC.DynFlags -> Outputable.SDoc -> String
showSDoc uq dyn = let style = Outputable.mkUserStyle uq Outputable.AllTheWay
                      cxt   = Outputable.initSDocContext dyn style
                  in Pretty.showDocWith Pretty.OneLineMode . flip Outputable.runSDoc cxt

