{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

import qualified GHC
import qualified Exception
import qualified Outputable
import qualified RdrName
import qualified OccName
import MonadUtils (MonadIO(liftIO))
import qualified GHC.Paths

import Control.Monad
import Control.Applicative

import System.IO
import System.Environment (getArgs)

import qualified Filesystem.Path.CurrentOS as P

import qualified Data.Aeson as Json
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Either
import Data.Maybe

import Hassistant.Directory
import Hassistant.Imports
import Hassistant.Module
import Hassistant.Common

data ExecMode = Normal
              | Debug
              deriving (Eq)

main :: IO ()
main = GHC.runGhc (Just GHC.Paths.libdir) $ liftIO getArgs >>= \case
    file:other -> do
        let execMode = case other of
                ["debug"] -> Debug
                _         -> Normal

        liftIO $ hSetBuffering stdout (BlockBuffering $ Just 100)
        dyn <- GHC.getSessionDynFlags
        (base,mbgpd) <- liftIO $ getBaseSrcAndCabal (P.decodeString file)

        let ps = maybe [] hsLibraryPackages mbgpd

        extraPC <- liftIO $ extraPkgConfs base
        _ <- GHC.setSessionDynFlags $
            dyn { GHC.extraPkgConfs = extraPC
                , GHC.pkgDatabase   = Nothing
                , GHC.ghcLink       = GHC.NoLink
                , GHC.packageFlags  = ps
                }

        is     <- imports <$> liftIO T.getContents
        let prelude = "import Prelude"
        
        idecls <- catMaybes <$> mapM (\i -> fmap Just (GHC.parseImportDecl i)
            `GHC.gcatch` (\(_::Exception.SomeException) -> return Nothing)) 
            (if execMode == Debug then is else prelude:is)

        ms <- catMaybes <$>
            mapM (\i -> (fmap (Just . (i,)) . findModule . GHC.unLoc . GHC.ideclName) i
            `GHC.gcatch` (\(_::Exception.SomeException) -> return Nothing)) idecls

        GHC.setContext $ map (GHC.IIDecl . GHC.simpleImportDecl . GHC.moduleName . snd) ms

        cs <- fmap concat $ forM ms $ \(idecl, mdl) -> GHC.getModuleInfo mdl >>= \case
            Just mi -> concat <$> mapM (\name -> GHC.lookupName name >>= \case
                Just tyThing -> do
                    let rdrs = rdrNames idecl name
                        mn   = ideclShortModuleName idecl
                    return $ mapMaybe (\rdr -> mkCandidate dyn mn rdr tyThing) rdrs
                Nothing -> return []
                ) (GHC.modInfoExports mi)
            Nothing -> return []

        let fs = lefts  cs
            ts = rights cs

        if execMode == Debug
            then mapM_ (liftIO . print) (fs ++ ts)
            else liftIO . L.putStrLn $ Json.encode (Dict kind $ fs ++ ts, fs, ts)

    _ -> liftIO $ putStrLn "USAGE: types file"

ideclShortModuleName :: GHC.ImportDecl t -> GHC.ModuleName
ideclShortModuleName GHC.ImportDecl{GHC.ideclName, GHC.ideclAs} = case ideclAs of
    Nothing -> GHC.unLoc ideclName
    Just n  -> n

rdrNames :: GHC.ImportDecl GHC.RdrName -> GHC.Name -> [GHC.RdrName]
rdrNames GHC.ImportDecl{GHC.ideclName, GHC.ideclQualified, GHC.ideclAs} name = case ideclAs of
    Nothing -> let qual = RdrName.Qual (GHC.unLoc ideclName) (OccName.occName name)
               in if ideclQualified 
                  then [qual]
                  else [RdrName.Unqual (OccName.occName name), qual]
    Just as -> let qual = RdrName.Qual as (OccName.occName name)
               in if ideclQualified
                  then [qual]
                  else [RdrName.Unqual (OccName.occName name), qual]

mkCandidate :: GHC.DynFlags -> GHC.ModuleName -> GHC.RdrName -> GHC.TyThing -> Maybe (Either Candidate Candidate)
mkCandidate dyn mdl rdr tyThing = case tyThing of
    (GHC.AnId     i) -> Just . Left $ wkm (ppr rdr) (pprType $ GHC.idType i) (ppr mdl)
    (GHC.ADataCon c) -> Just . Left $ wkm (ppr rdr) (pprType $ GHC.dataConType c) (ppr $ GHC.dataConTyCon c)
    (GHC.ATyCon   c) -> case GHC.tyConClass_maybe c of
        Nothing  -> Just . Right $ wkm (ppr rdr) (ppConc . map ppr $ GHC.tyConDataCons c)  (ppr mdl)
        Just cls -> Just . Right $ wkm (ppr rdr) (ppConc . map ppr $ GHC.classMethods cls) (ppr mdl)
    _ -> Nothing
  where
    wkm w k m = (candidate w) {kind = Just k, menu = Just m}
    ppConc s  = '(' `T.cons` T.intercalate "," s `T.snoc` ')'
    ppr       = T.pack . showSDoc Outputable.neverQualify dyn . Outputable.ppr
    pprType   = ppr . snd . GHC.splitForAllTys

