{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import qualified GHC
import qualified Exception
import qualified Outputable
import qualified HscTypes
import qualified RdrName
import qualified Name
import qualified OccName
import MonadUtils (MonadIO(liftIO))
import qualified GHC.Paths

import Control.Applicative

import System.IO
import System.Environment (getArgs)

import qualified Filesystem.Path.CurrentOS as P

import qualified Data.Aeson as Json
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Maybe

import Hassistant.Directory
import Hassistant.Imports
import Hassistant.Module
import Hassistant.Common

main :: IO ()
main = GHC.runGhc (Just GHC.Paths.libdir) $ liftIO getArgs >>= \case
    [file] -> do
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
        
        idecls <- catMaybes <$> mapM (\i -> fmap Just (GHC.parseImportDecl i)
            `GHC.gcatch` (\(_::Exception.SomeException) -> return Nothing)) is

        ms <- catMaybes <$> mapM (\i -> (fmap Just . findModule . GHC.unLoc . GHC.ideclName) i
            `GHC.gcatch` (\(_::Exception.SomeException) -> return Nothing)) idecls

        GHC.setContext $ GHC.IIDecl (GHC.simpleImportDecl $ GHC.mkModuleName "Prelude"): 
            map (GHC.IIDecl . GHC.simpleImportDecl . GHC.moduleName) ms

        uq <- GHC.getPrintUnqual

        sess <- GHC.getSession
        let gre = HscTypes.ic_rn_gbl_env $ HscTypes.hsc_IC sess 

        GHC.getNamesInScope >>= mapM (mkCandidate uq dyn gre) >>=
                liftIO . L.putStrLn . Json.encode . concat

    _ -> liftIO $ putStrLn "USAGE: types file"

mkCandidate :: GHC.GhcMonad m => Outputable.PrintUnqualified -> GHC.DynFlags
            -> OccName.OccEnv [RdrName.GlobalRdrElt] -> GHC.Name -> m [Json.Value]
mkCandidate uq dyn gre name = do
    let rdr = concatMap greToRdrNames $ RdrName.lookupGRE_Name gre name
    GHC.lookupName name >>= \case
        Just (GHC.AnId i)     -> return $
            map (\n -> Json.object [ "word" Json..= ppr n
                                   , "kind" Json..= pprType (GHC.idType i)
                                   , "menu" Json..= ppr (GHC.nameModule name)
                                   ]) rdr
        Just (GHC.ADataCon c) -> return $
            map (\n -> Json.object [ "word" Json..= ppr n
                                   , "kind" Json..= pprType (GHC.dataConType c)
                                   , "menu" Json..= ppr (GHC.dataConTyCon c)
                                   ]) rdr
        Just (GHC.ATyCon c)   -> case GHC.tyConClass_maybe c of 
            Nothing  -> return $
                map (\n -> Json.object [ "word" Json..= ppr n
                                       , "kind" Json..= ppr (GHC.tyConDataCons c)
                                       , "menu" Json..= Json.String "[TyCon]"
                                       ]) rdr
            Just cls -> return $
                map (\n -> Json.object [ "word" Json..= ppr n
                                       , "kind" Json..= ppr (GHC.classMethods cls)
                                       , "menu" Json..= Json.String "[Class]"
                                       ]) rdr
        _ -> return []

  where
    ppr = Json.String . T.pack . showSDoc uq dyn . Outputable.ppr
    pprType = ppr . snd . GHC.splitForAllTys

-- copy from InteractiveEval
greToRdrNames :: RdrName.GlobalRdrElt -> [GHC.RdrName]
greToRdrNames RdrName.GRE{ RdrName.gre_name = name, RdrName.gre_prov = prov }
  = case prov of
     RdrName.LocalDef -> [unqual]
     RdrName.Imported specs -> concat (map do_spec (map RdrName.is_decl specs))
  where
    occ = Name.nameOccName name
    unqual = RdrName.Unqual occ
    do_spec decl_spec
        | RdrName.is_qual decl_spec = [qual]
        | otherwise                 = [unqual,qual]
        where qual = RdrName.Qual (RdrName.is_as decl_spec) occ
