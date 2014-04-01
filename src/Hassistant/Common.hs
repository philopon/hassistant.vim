{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Hassistant.Common where

import qualified GHC
import qualified Outputable
import qualified Pretty

import Control.Applicative

import Foreign.C

import qualified Data.ByteString.Unsafe as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Aeson as Json
import Data.Maybe

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

data Candidate = Candidate 
    { word ::       T.Text
    , abbr :: Maybe T.Text
    , kind :: Maybe T.Text
    , menu :: Maybe T.Text
    , info :: Maybe T.Text
    , rank :: Maybe Int
    } deriving Show

candidate :: T.Text -> Candidate
candidate w = Candidate w Nothing Nothing Nothing Nothing Nothing

instance Json.ToJSON Candidate where
    toJSON Candidate{..} =
        Json.object $ (:) ("word" Json..= word) $
            maybe id (add "abbr") abbr $
            maybe id (add "kind") kind $
            maybe id (add "menu") menu $
            maybe id (add "info") info $
            maybe id (add "rank") rank $ []
      where add s = ((:) . (s Json..=))

data Dict = Dict (Candidate -> Maybe T.Text) [Candidate]

instance Json.ToJSON Dict where
    toJSON (Dict a cs) = Json.object $ mapMaybe object cs
      where 
        object c = (word c Json..=) <$> (a c)
