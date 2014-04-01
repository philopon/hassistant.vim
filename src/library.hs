{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Library where

import Foreign.C (CInt(..), CString, peekCString)
import qualified Foreign.Storable as Storable
import qualified Foreign.Marshal as Marshal

import System.IO.Unsafe(unsafePerformIO)
import qualified Filesystem.Path.CurrentOS as P

import Control.Applicative
import Control.Concurrent
import Control.Exception

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Unsafe as U
import qualified Data.Aeson   as Json
import Data.Word (Word8)
import Data.List
import qualified Data.Attoparsec.Text as A

import Hassistant.Imports
import Hassistant.Common
import Hassistant.Parser
import Hassistant.LANGUAGE
import Hassistant.Module

#ifdef MODULE
foreign export ccall destruct                 :: IO ()

foreign export ccall hashFile                 :: CFilePath -> IO CInt
foreign export ccall hash                     :: CString   -> IO CInt

foreign export ccall position                 :: CString   -> IO CString

foreign export ccall gatherLANGUAGE           :: CString   -> IO CString
foreign export ccall gatherModule             :: CString   -> IO CString
foreign export ccall gatherNamesInModule      :: CString   -> IO CString
foreign export ccall gatherNamesInConstructor :: CString   -> IO CString
foreign export ccall gatherTopLevel           :: CString   -> IO CString
#endif

type CFilePath = CString

destructors :: MVar [IO ()]
destructors = unsafePerformIO $ newMVar []

newCStringFromBS :: S.ByteString -> IO CString
newCStringFromBS bs = U.unsafeUseAsCStringLen bs $ \(src, len) -> do
    dst <- Marshal.mallocBytes (succ len)
    Marshal.copyBytes dst src len
    Storable.pokeByteOff dst len (0::Word8)
    modifyMVar_ destructors (return . (Marshal.free dst:))
    return dst

destruct :: IO ()
destruct = modifyMVar_ destructors $ \l -> sequence_ l >> return []

--------------------------------------------------------------------------------

hashFile :: CFilePath -> IO CInt
hashFile cfile = do
    file <- peekCString cfile
    fromIntegral <$> (importHash =<< T.readFile file)

hash :: CString -> IO CInt
hash ccont = do
    cont <- unsafePackCStringToText ccont
    fromIntegral <$> importHash cont

--------------------------------------------------------------------------------

positionP :: A.Parser S.ByteString
positionP = 
    (positionMode 1        <$> positionLanguageP) <|>
    (positionNamesInConst  <$> positionNamesInConstructorP) <|>
    (positionNamesInModule <$> positionNamesInModuleP) <|>
    (positionMode 4        <$> positionModuleP) <|>
    (positionMode 5        <$> (0 <$ positionTopLevelP))
  where
    positionNamesInModule (m,i) = L.toStrict . Json.encode . Json.object $
        [ "mode"     Json..= (2 :: Int)
        , "position" Json..= i
        , "module"   Json..= m 
        ]
    positionNamesInConst (m,n,i) = L.toStrict . Json.encode . Json.object $
        [ "mode"        Json..= (3 :: Int)
        , "position"    Json..= i
        , "module"      Json..= m
        , "constructor" Json..= n
        ]

positionMode :: Int -> Int -> S.ByteString
positionMode m p = L.toStrict . Json.encode . Json.object $
    ["mode" Json..= m, "position" Json..= p]

position :: CString -> IO CString
position cstr = do
    txt <- unsafePackCStringToText cstr
    case A.parseOnly positionP txt of
        Right r -> newCStringFromBS r
        Left  _ -> newCStringFromBS $ positionMode 0 (-1)

gatherLANGUAGE :: CString -> IO CString
gatherLANGUAGE _ = newCStringFromBS . L.toStrict $ Json.encode listLANGAUGE

gatherModule :: CString -> IO CString
gatherModule file = do
    p  <- P.fromText <$> unsafePackCStringToText file
    newCStringFromBS . L.toStrict . Json.encode . map (uncurry cand) =<< 
        listModule p `catch` (\(_::SomeException) -> return [])
  where
    wkm w k m = (candidate $ T.pack w) { kind = Just $ T.pack k, menu = Just m }
    cand mdl Nothing    =  wkm mdl "(file)" "[Module]"
    cand mdl (Just pkg) = (wkm mdl pkg      "[Module]") { rank = Just 250 }

gatherNamesInModule :: CString -> IO CString
gatherNamesInModule query = do
    lbs <- (T.lines <$> unsafePackCStringToText query) >>= \case
        [file, mdl] -> Json.encode . concatMap cand <$>
            listNamesInModule (P.fromText file) mdl `catch` (\(_::SomeException) -> return [])
        _           -> return $ Json.encode ([] :: [Json.Value])
    newCStringFromBS $ L.toStrict lbs
  where
    wkm w k m = (candidate $ T.pack w) { kind = Just $ T.pack k, menu = Just m }
    ppDC s = '(' : (intercalate "," s) ++ ")"

    cand (Var         n t ) = [wkm n t "[Function]"]
    cand (Constructor t ds) = wkm t (ppDC $ map fst ds) "[TyCon]" : 
                              map (\(d,_) -> wkm d t "[DataCon]") ds
    cand (Class       c ms) = wkm c (ppDC $ map fst ms) "[Class]" : 
                              map (\(d,t) -> wkm d t (T.pack c)) ms

gatherNamesInConstructor :: CString -> IO CString
gatherNamesInConstructor query = do
    lbs <- (T.lines <$> unsafePackCStringToText query) >>= \case
        [file, mdl, cnst] -> Json.encode <$> 
            (maybe [] cand <$> listNamesInConstructor (P.fromText file) mdl (T.unpack cnst))
            `catch` (\(_::SomeException) -> return [])

        _           -> return $ Json.encode ([] :: [Json.Value])
    newCStringFromBS $ L.toStrict lbs
  where
    wkm w k m = (candidate $ T.pack w) { kind = Just $ T.pack k, menu = Just $ T.pack m }
    cand (Var         _ _ ) = []
    cand (Constructor c ds) = map (\(d,t) -> wkm d t c) ds
    cand (Class       c ms) = map (\(m,t) -> wkm m t c) ms

gatherTopLevel :: CString -> IO CString
gatherTopLevel _ = newCStringFromBS . L.toStrict . Json.encode $ map cand topLevels
  where
    cand c = (candidate c){ menu = Just "[top]" }
