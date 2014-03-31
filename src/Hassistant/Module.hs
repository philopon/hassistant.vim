{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Hassistant.Module where

import qualified GHC
import MonadUtils (MonadIO(liftIO))
import qualified Finder
import qualified RdrName
import qualified OccName
import qualified HscTypes
import qualified DynFlags
import qualified Module
import qualified GHC.Paths
import qualified Outputable

import Control.Monad
import Control.Applicative

import qualified Filesystem.Path.CurrentOS as P

import qualified Distribution.ModuleName as D
import qualified Distribution.PackageDescription as D
import qualified Distribution.Text as D

import qualified Data.Text as T
import Data.Maybe

import Hassistant.Directory
import Hassistant.Common

listModule' :: GHC.GhcMonad m => GHC.DynFlags -> P.FilePath -> m ([D.ModuleName], [GHC.Module])
listModule' dyn file = do
    (base, gpd) <- liftIO $ getBaseSrcAndCabal file
    extraPC     <- liftIO $ extraPkgConfs base
    _ <- GHC.setSessionDynFlags $
        dyn { GHC.extraPkgConfs = extraPC
            , GHC.pkgDatabase   = Nothing
            , GHC.ghcLink       = GHC.LinkInMemory
            }

    let cbl = case join $ D.condLibrary <$> gpd of
            Just cl -> D.libModules $ D.condTreeData cl
            Nothing -> []

    (cbl,) <$> GHC.packageDbModules False

listModule :: P.FilePath -> IO [(String, Maybe String)]
listModule file = GHC.runGhc (Just GHC.Paths.libdir) $ do
    dyn      <- GHC.getSessionDynFlags
    (cs, ms) <- listModule' dyn file
    return $ map cabalElem cs ++ map (moduleElem dyn) ms
  where
    ppr dyn = showSDoc Outputable.neverQualify dyn . Outputable.ppr
    moduleElem dyn mdl =
        let moduleName = ppr dyn $ GHC.moduleName      mdl
            packageId  = ppr dyn $ GHC.modulePackageId mdl
        in (moduleName, Just packageId)
    cabalElem mdl = (D.display mdl, Nothing)

--------------------------------------------------------------------------------

data NamesInModule t n 
    = Constructor { getName  :: n
                  , dataCons :: [(n, t)]
                  }
    | Class   { getName  :: n
              , classFns :: [(n, t)]
              }
    | Var     { getName  :: n
              , varType  :: t
              }
    deriving (Show, Functor)

fmapTypes :: (t -> t') -> NamesInModule t n -> NamesInModule t' n
fmapTypes f (Constructor n d) = Constructor n $ map (\(l,t) -> (l, f t)) d
fmapTypes f (Class n m)       = Class n $ map (\(l,t) -> (l, f t)) m
fmapTypes f (Var n t)         = Var n (f t)

findModule :: GHC.GhcMonad m => GHC.ModuleName -> m GHC.Module
findModule mName = do
    sess <- GHC.getSession
    liftIO (Finder.findImportedModule sess mName Nothing) >>= \case
        Finder.Found _ mdl     -> return mdl
        Finder.NoPackage{}     -> fail "package not found"
        Finder.FoundMultiple{} -> fail "found multiple"
        Finder.NotFound{Finder.fr_pkgs_hidden} -> do
            _ <- GHC.setSessionDynFlags $ (HscTypes.hsc_dflags sess)
                { GHC.packageFlags = map (DynFlags.ExposePackage . Module.packageIdString) fr_pkgs_hidden }
            GHC.findModule mName Nothing

gListModule :: GHC.GhcMonad m => (GHC.ModuleInfo -> m a) -> GHC.DynFlags -> P.FilePath -> T.Text -> m a
gListModule fpkg dyn file mtxt = do
    (base, mbgpd) <- liftIO $ getBaseSrcAndCabal file
    let mName = GHC.mkModuleName $ T.unpack mtxt

    let ps = maybe [] hsLibraryPackages mbgpd

    extraPC <- liftIO $ extraPkgConfs base
    _ <- GHC.setSessionDynFlags $
        dyn { GHC.extraPkgConfs = extraPC
            , GHC.pkgDatabase   = Nothing
            , GHC.ghcLink       = GHC.NoLink
            , GHC.packageFlags  = ps
            }

    mdl <- findModule mName

    GHC.setContext [GHC.IIDecl . GHC.simpleImportDecl $ GHC.moduleName mdl]
    GHC.getModuleInfo mdl >>= \case
        Just mi -> fpkg mi
        Nothing -> fail "no module info"

tyThingToNamesInModule :: GHC.TyThing -> Maybe (NamesInModule GHC.Type GHC.RdrName)
tyThingToNamesInModule = \case
    (GHC.AnId     v) -> Just $ Var (rdrName v) (GHC.idType v)
    (GHC.ADataCon _) -> Nothing
    (GHC.ATyCon   t) -> Just $ case GHC.tyConClass_maybe t of
        Nothing  -> Constructor (rdrName t) $ map (\d -> (rdrName d, GHC.dataConType d)) (GHC.tyConDataCons t)
        Just cls -> Class       (rdrName t) $ map (\v -> (rdrName v, GHC.idType v))      (GHC.classMethods cls)
    (GHC.ACoAxiom _) -> Nothing
  where
    rdrName = RdrName.mkRdrUnqual . GHC.getOccName

parenHasOccName :: OccName.HasOccName n => n -> Outputable.SDoc
parenHasOccName ho = OccName.parenSymOcc occ (Outputable.ppr occ)
  where
    occ = OccName.occName ho

listNamesInModule' :: GHC.GhcMonad m => GHC.DynFlags -> P.FilePath -> T.Text -> m [NamesInModule String String]
listNamesInModule' = gListModule $ \mi -> do
    dyn <- GHC.getSessionDynFlags
    let xp = GHC.modInfoExports mi
    tyThings <- catMaybes <$> mapM (GHC.modInfoLookupName mi) xp
    Just uq <- GHC.mkPrintUnqualifiedForModule mi
    return $ mapMaybe (fmap (ppr uq dyn) . tyThingToNamesInModule) tyThings
  where
    ppr     uq dyn = fmapTypes (pprType uq dyn) . fmap (showSDoc uq dyn . parenHasOccName)
    pprType uq dyn = showSDoc uq dyn . Outputable.ppr . snd . GHC.splitForAllTys

listNamesInModule :: P.FilePath -> T.Text -> IO [NamesInModule String String]
listNamesInModule file mdl = GHC.runGhc (Just GHC.Paths.libdir) $ do
    dyn <- GHC.getSessionDynFlags
    listNamesInModule' dyn file mdl

listNamesInConstructor :: P.FilePath -> T.Text -> String -> IO (Maybe (NamesInModule String String))
listNamesInConstructor file mdl cnst = GHC.runGhc (Just GHC.Paths.libdir) $ do
    dyn <- GHC.getSessionDynFlags
    filter ((== cnst) . getName) <$> listNamesInModule' dyn file mdl >>= \case
        [a] -> return $ Just a
        _   -> return Nothing

