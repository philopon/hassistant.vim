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
import qualified Data.Attoparsec.Text as A

import Hassistant.Imports
import Hassistant.Common
import Hassistant.Parser
import Hassistant.LANGUAGE
import Hassistant.Module

#ifdef MODULE
foreign export ccall destruct            :: IO ()

foreign export ccall hashFile            :: CFilePath -> IO CInt
foreign export ccall hash                :: CString   -> IO CInt

foreign export ccall position            :: CString -> IO CString

foreign export ccall gatherLANGUAGE           :: CString -> IO CString
foreign export ccall gatherModule             :: CString -> IO CString
foreign export ccall gatherNamesInModule      :: CString -> IO CString
foreign export ccall gatherNamesInConstructor :: CString -> IO CString
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
    (positionMode 4        <$> positionModuleP)
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
gatherLANGUAGE _ = newCStringFromBS neocompleteLANGAUGE

gatherModule :: CString -> IO CString
gatherModule file = do
    p  <- P.fromText <$> unsafePackCStringToText file
    newCStringFromBS . L.toStrict . Json.encode . map ppMod =<< 
        listModule p `catch` (\(_::SomeException) -> return [])
  where
    ppMod (m, mbP) = Json.object .
                     maybe (("rank" Json..= (250::Int)):) (const id) mbP $
                     [ "word" Json..= m
                     , "kind" Json..= maybe "(file)" id mbP
                     , "menu" Json..= Json.String "[Module]"
                     ]

gatherNamesInModule :: CString -> IO CString
gatherNamesInModule query = do
    lbs <- (T.lines <$> unsafePackCStringToText query) >>= \case
        [file, mdl] -> Json.encode . concatMap ppName <$>
            listNamesInModule (P.fromText file) mdl `catch` (\(_::SomeException) -> return [])
        _           -> return $ Json.encode ([] :: [Json.Value])
    newCStringFromBS $ L.toStrict lbs
  where
    menu s = "menu" Json..= Json.String ('[' `T.cons` s `T.snoc` ']')
    ppDC s = '(' `T.cons` T.intercalate "," (map T.pack s) `T.snoc` ')'
    ppName (Var n t) =
        [Json.object [ "word" Json..= n, menu "Variable", "kind" Json..= T.pack t]]
    ppName (Constructor t ds) =
        Json.object [ "word" Json..= T.pack t, menu "TyCon", "kind" Json..= ppDC (map fst ds)] :
        map (\(d,_) -> Json.object ["word" Json..= T.pack d, menu "DataCon", "kind" Json..= T.pack t]) ds
    ppName (Class c m) =
        Json.object [ "word" Json..= T.pack c, menu "Class", "kind" Json..= ppDC (map fst m)] :
        map (\(d,t) -> Json.object ["word" Json..= T.pack d, menu (T.pack c), "kind" Json..= T.pack t]) m

gatherNamesInConstructor :: CString -> IO CString
gatherNamesInConstructor query = do
    lbs <- (T.lines <$> unsafePackCStringToText query) >>= \case
        [file, mdl, cnst] -> Json.encode <$> 
            (maybe [] ppName <$> listNamesInConstructor (P.fromText file) mdl (T.unpack cnst))
            `catch` (\(_::SomeException) -> return [])

        _           -> return $ Json.encode ([] :: [Json.Value])
    newCStringFromBS $ L.toStrict lbs
  where
    menu s = "menu" Json..= Json.String ('[' `T.cons` s `T.snoc` ']')
    ppName (Var _ _) = []
    ppName (Constructor c ds) =
        map (\(d,t) -> Json.object ["word" Json..= d, menu (T.pack c), "kind" Json..= T.pack t]) ds
    ppName (Class c ms) =
        map (\(m,t) -> Json.object ["word" Json..= m, menu (T.pack c), "kind" Json..= T.pack t]) ms
