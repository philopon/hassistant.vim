{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Hassistant.LANGUAGE
    ( listLANGUAGE
    , neocompleteLANGAUGE
    ) where

import qualified DynFlags

import Language.Haskell.TH

import qualified Data.Aeson as A

import qualified Data.ByteString as S
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

listLANGUAGE :: [String]
listLANGUAGE = concatMap (\(s,_,_) -> ["No" ++ s, s]) DynFlags.xFlags

neocompleteLANGAUGE :: S.ByteString
neocompleteLANGAUGE = $(
    let obj s = A.object [ "word" A..= s, "menu" A..= A.String "[LANGAUGE]" ]
    in stringE . TL.unpack . TL.decodeUtf8 . A.encode $
       concatMap (\(s,_,_) -> [obj s, obj $ "No" ++ s]) DynFlags.xFlags
       )

