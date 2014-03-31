{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hassistant.Directory where

import qualified DynFlags

import Control.Monad
import Control.Applicative
import Control.Exception

import qualified Filesystem as P
import qualified Filesystem.Path.CurrentOS as P

import qualified Distribution.PackageDescription as D
import qualified Distribution.PackageDescription.Parse as D
import qualified Distribution.ModuleName as D
import qualified Distribution.Verbosity as D
import qualified Distribution.Package as D

import qualified Data.Text as T
import qualified Data.Char as Char
import qualified Data.Attoparsec.Text as A
import Data.Either

import Hassistant.Common
import Hassistant.Parser

listDirectory' :: P.FilePath -> IO [P.FilePath]
listDirectory' = P.listDirectory >=> mapM (\d -> P.isDirectory d >>= return . bool (d P.</> "") d)

dirOrFileName :: P.FilePath -> P.FilePath
dirOrFileName f = let fn = P.filename f in bool (P.dirname f) fn (P.null fn)

toAbsolutePath :: P.FilePath -> IO P.FilePath
toAbsolutePath file | P.absolute file = return file 
                    | otherwise       = fmap P.collapse $ (P.</>) <$> P.getWorkingDirectory <*> pure file

seekUp :: (P.FilePath -> Bool) -> P.FilePath -> IO (Maybe P.FilePath)
seekUp p file = toAbsolutePath file >>= go . P.directory 
  where 
    go dir = filter p <$> listDirectory' dir >>= \case
        []  -> let parent = P.parent dir
               in bool (return Nothing) (go parent) (dir == parent)
        a:_ -> return (Just a)

getBaseSrcAndCabal :: P.FilePath -> IO (P.FilePath, Maybe D.GenericPackageDescription)
getBaseSrcAndCabal file = seekUp ((== Just "cabal") . P.extension) file >>= \case
    Just cabal -> do
        gpd <- D.readPackageDescription D.silent (P.encodeString cabal)
        return (P.directory cabal, Just gpd)
    Nothing -> seekUp ((== "cabal.sandbox.config") . P.filename) file >>= \case
        Just sandbox -> return (P.directory sandbox, Nothing)
        Nothing      -> parseFromFile
  where parseFromFile = do
            absfile <- toAbsolutePath file
            let idPath = (P.parent absfile, Nothing)
            do A.parseOnly moduleP <$> P.readTextFile file >>= \case
                 Left _  -> return idPath
                 Right m -> return (up (T.count "." m) absfile, Nothing)
              `catch` (\(_::SomeException) -> return idPath)
        up 0 f = P.parent f
        up i f = up (pred i) $ P.parent f

packageDB :: T.Text -> Maybe P.FilePath
packageDB txt = case filter (T.isPrefixOf "package-db:") $ T.lines txt of
    line:_ -> Just . P.fromText . T.dropWhile Char.isSpace $ T.drop 11 line
    _      -> Nothing

extraPkgConfs :: P.FilePath -> IO ([DynFlags.PkgConfRef] -> [DynFlags.PkgConfRef])
extraPkgConfs base = do 
    e <- P.isFile sandbox
    if e
        then packageDB <$> P.readTextFile sandbox >>= \case
            Nothing -> return $ \old -> DynFlags.GlobalPkgConf:old
            Just db -> do
                return $ \old -> DynFlags.GlobalPkgConf:DynFlags.PkgConfFile (P.encodeString db):old
        else return $ \old -> DynFlags.GlobalPkgConf:DynFlags.UserPkgConf:old
   where sandbox = base P.</> "cabal.sandbox.config"

pathToModuleName :: P.FilePath -> Maybe T.Text
pathToModuleName file = do 
    let ts = map P.toText . P.splitDirectories $ P.dropExtensions file
    if all isRight ts
        then Just . T.intercalate "." $ rights ts
        else Nothing
  where 
    isRight (Right s) | Char.isUpper $ T.head s = True
    isRight _                                   = False

moduleNameToPath :: T.Text -> P.FilePath
moduleNameToPath = P.concat . map P.fromText . T.split (== '.')

hsLibraryFiles :: P.FilePath -> D.GenericPackageDescription -> IO [P.FilePath]
hsLibraryFiles base gpd = case D.condLibrary gpd of
    Nothing -> return []
    Just cl -> do
        let srcs = D.hsSourceDirs . D.libBuildInfo $ D.condTreeData cl
            mdls = map (P.decodeString . D.toFilePath) . D.libModules $ D.condTreeData cl
            allf = [ base P.</> P.decodeString src P.</> mn P.<.> ext
                   | src <- srcs
                   , mn  <- mdls
                   , ext <- ["hs", "hsc", "chs", "lhs"]
                   ]
        filterM P.isFile allf

hsLibraryPackages :: D.GenericPackageDescription -> [DynFlags.PackageFlag]
hsLibraryPackages gpd = case D.condLibrary gpd of
    Nothing -> []
    Just cl ->
        let pkgName (D.Dependency (D.PackageName n) _) = n
            deps = map pkgName $ D.condTreeConstraints cl
        in map DynFlags.ExposePackage deps


