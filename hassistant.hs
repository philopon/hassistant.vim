{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ForeignFunctionInterface  #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE CPP                       #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Hassistant where

import qualified GHC
import qualified GhcMonad
import qualified Outputable
import qualified MonadUtils
import qualified Parser 
import qualified Pretty
import qualified Packages
import qualified Lexer
import qualified DynFlags
import qualified StringBuffer
import qualified SrcLoc
import qualified HscTypes
import qualified FastString
import qualified HsSyn
import qualified Module
import qualified Finder
import qualified HsImpExp
import qualified RdrName
import qualified Type
import qualified TcRnDriver
import qualified Name
import qualified HsExpr
import qualified GHC.Paths

import qualified Data.ByteString            as S
import qualified Data.ByteString.Unsafe     as S
import qualified Data.ByteString.Char8      as SC
import qualified Data.ByteString.Lazy.Char8 as L

import qualified Data.Text                  as T
import qualified Data.Text.IO               as T
import qualified Data.Text.Encoding         as T

import qualified Data.HashMap.Strict        as H
import qualified Data.Hashable              as Hash
import qualified Data.IORef                 as IORef
import qualified Data.Attoparsec            as P
import qualified Data.Attoparsec.Char8      as PC
import qualified Data.Aeson                 as J
import           Data.List
import           Data.Maybe
import           Data.Either

import qualified System.IO.Unsafe
import qualified System.Directory           as D
import qualified System.FilePath            as F

import qualified Foreign.C                  as C
import qualified Foreign.Marshal            as C
import qualified Foreign.Storable           as C

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.Maybe
import           Control.Monad.IO.Class
import           Control.Exception

#ifdef MODULE
foreign export ccall listLANGUAGE      :: C.CString -> IO C.CString
foreign export ccall getRoot           :: C.CString -> IO C.CString
foreign export ccall listModule        :: C.CString -> IO C.CString
foreign export ccall listAllNames      :: C.CString -> IO C.CString
foreign export ccall listNamesInModule :: C.CString -> IO C.CString
foreign export ccall queryHash         :: C.CString -> IO C.CInt
foreign export ccall destruct          :: IO ()
#endif

destructors :: IORef.IORef [IO ()]
destructors = System.IO.Unsafe.unsafePerformIO $ IORef.newIORef []

destruct :: IO ()
destruct = IORef.readIORef destructors >>= sequence_

newCStringFromBS :: S.ByteString -> IO C.CString
newCStringFromBS bs = S.unsafeUseAsCStringLen bs $ \(cstr, len) -> do
    mem <- C.mallocBytes (succ len)
    IORef.atomicModifyIORef destructors (\ref -> (C.free mem:ref, ()))
    C.copyBytes mem cstr len
    C.pokeByteOff mem len (0::C.CUChar)
    return mem

newCString :: String -> IO C.CString
newCString s = C.newCString s >>= \mem ->
    IORef.atomicModifyIORef destructors (\ref -> (C.free mem: ref, ())) >> return mem

--------------------------------------------------------------------------------

ghcArgs :: FilePath -> IO ([DynFlags.PkgConfRef] -> [DynFlags.PkgConfRef])
ghcArgs root = do
    db <- D.doesFileExist sandboxConf >>= \e ->
          if e
              then getSandboxPackageDB sandboxConf >>= \case
                  Nothing -> return id
                  Just db -> return $ const [DynFlags.GlobalPkgConf, DynFlags.PkgConfFile db]
              else return id
    return db
  where sandboxConf = root F.</> "cabal.sandbox.config"

runGHC :: FilePath -> [OnOff DynFlags.ExtensionFlag] -> GHC.Ghc b -> IO (Maybe b)
runGHC file xflags m = (Just <$> go) `catch` (\(_ :: SomeException) -> return Nothing)
  where
    go = do
        root          <- getRoot' file
        extraPkgConfs <- maybe (return id) ghcArgs root
        GHC.runGhc (Just GHC.Paths.libdir) $ do
            (dflags, _) <-
                MonadUtils.liftIO . Packages.initPackages . applyXFlags xflags .
                    (\d -> d { GHC.extraPkgConfs = extraPkgConfs
                             , GHC.pkgDatabase   = Nothing
                             , GHC.ghcLink       = GHC.LinkInMemory
                             } ) =<< GHC.getSessionDynFlags
            void $ GHC.setSessionDynFlags dflags
            m

--------------------------------------------------------------------------------

sdocToString :: GhcMonad.GhcMonad m => Outputable.SDoc -> m String
sdocToString sdoc = GhcMonad.withSession $ \sess -> do
    let gre = RdrName.mkGlobalRdrEnv . RdrName.globalRdrEnvElts $ HscTypes.ic_rn_gbl_env $ HscTypes.hsc_IC sess
        df  = HscTypes.hsc_dflags sess
    let pu  = HscTypes.mkPrintUnqualified df gre
    return . Pretty.showDocWith Pretty.OneLineMode $ Outputable.runSDoc sdoc
        (Outputable.initSDocContext df $ Outputable.mkUserStyle pu Outputable.AllTheWay)

--------------------------------------------------------------------------------

listLANGUAGE' :: T.Text -> [H.HashMap T.Text T.Text]
listLANGUAGE' pfx = 
    filter ((pfx `T.isPrefixOf`) . (H.! "word")) $
    concatMap (\(w,_,_) -> [mkLANG $ T.pack w, mkLANG . T.pack $ "No" ++ w]) DynFlags.xFlags
  where mkLANG w = H.fromList [("word", w), ("menu", "[LANGUAGE]")]


listLANGUAGE :: C.CString -> IO C.CString
listLANGUAGE pfx = T.decodeUtf8 <$> S.unsafePackCString pfx >>= \str ->
    (newCStringFromBS . L.toStrict . J.encode . listLANGUAGE') str

--------------------------------------------------------------------------------

getRoot' :: FilePath -> IO (Maybe FilePath)
getRoot' file = D.doesDirectoryExist file >>= \d -> runMaybeT $ go (if d then file else F.takeDirectory file)
  where 
    go "/" = MaybeT $ return Nothing
    go dir = do 
        c <- liftIO $ D.getDirectoryContents dir
        if null $ filter rootFile c
            then go $ F.takeDirectory dir
            else return dir

    rootFile "cabal.sandbox.config"            = True
    rootFile f | F.takeExtension f == ".cabal" = True
               | otherwise                     = False

getRoot :: C.CString -> IO C.CString
getRoot cfile = 
    C.peekCString cfile >>= getRoot' >>= 
        newCString . fromMaybe ""

getSandboxPackageDB :: FilePath -> IO (Maybe FilePath)
getSandboxPackageDB sandboxConf =
    fmap (T.unpack . T.dropWhile (== ' ') . T.drop 11) . listToMaybe . filter ("package-db" `T.isPrefixOf`) . map (T.dropWhile (== ' ')) . T.lines <$> T.readFile sandboxConf

--------------------------------------------------------------------------------

listModule' :: GHC.Ghc [H.HashMap T.Text String]
listModule' = do
    rdrNames <- GHC.packageDbModules True
    mapM parElem rdrNames
  where 
    parElem mdl = do
        word <- sdocToString (Outputable.ppr $ GHC.moduleName mdl)
        kind <- sdocToString (Outputable.ppr $ GHC.modulePackageId mdl)
        return $ H.fromList [("word", word), ("kind", kind), ("menu", "[Module]")]

listModule :: C.CString -> IO C.CString
listModule cfile =  do
    file <- C.peekCString cfile
    runGHC file [] listModule' >>= \case
        Nothing  -> newCString "[]"
        Just mdl -> newCStringFromBS . L.toStrict $ J.encode mdl

--------------------------------------------------------------------------------

readModule :: DynFlags.DynFlags -> FilePath -> String -> GHC.Ghc (Either String (HsSyn.LImportDecl GHC.RdrName))
readModule df fn str = case Lexer.unP Parser.parseModule st of
    Lexer.PFailed _ m -> Left <$> sdocToString m
    Lexer.POk _ (SrcLoc.L _ m) -> return $ case HsSyn.hsmodImports m of
        []  -> Left "No Imports"
        h:_ -> Right h
  where st = Lexer.mkPState df (StringBuffer.stringToStringBuffer str)
             (SrcLoc.mkRealSrcLoc (FastString.mkFastString fn) 0 0)

parseModule :: FilePath -> String -> GHC.Ghc (Either String (HsSyn.ImportDecl GHC.RdrName))
parseModule fn str = GhcMonad.withSession $ \sess ->
    readModule (HscTypes.hsc_dflags sess) fn str >>= \case
        Left e -> return $ Left e
        Right (SrcLoc.L _ a@HsSyn.ImportDecl 
            { HsSyn.ideclName = SrcLoc.L _ n
            , HsSyn.ideclPkgQual = pkg
            }) -> do
                MonadUtils.liftIO $ Finder.findExposedPackageModule sess n pkg >>= return . \case
                    Finder.Found _ _       -> Right a
                    Finder.NoPackage _     -> Left "No Package"
                    Finder.FoundMultiple _ -> Left "Found Multiple"
                    Finder.NotFound{}      -> Left "Not Found"
            
typeOf :: GhcMonad.GhcMonad m => HscTypes.HscEnv -> Name.Name -> m (Maybe Type.Type)
typeOf sess nm = MonadUtils.liftIO $
    snd <$> TcRnDriver.tcRnExpr sess 
    (HscTypes.emptyInteractiveContext $ HscTypes.hsc_dflags sess)
    (SrcLoc.noLoc . HsExpr.HsVar $ RdrName.Exact nm)

-- | copy from InteractiveEval.hs
greToRdrNames :: RdrName.GlobalRdrElt -> [RdrName.RdrName]
greToRdrNames RdrName.GRE {RdrName.gre_name = name, RdrName.gre_prov = prov } 
    = case prov of
        RdrName.LocalDef       -> [unqual]
        RdrName.Imported specs -> concat (map do_spec (map RdrName.is_decl specs))
  where occ = Name.nameOccName name
        unqual = RdrName.Unqual occ
        do_spec decl_spec
            | RdrName.is_qual decl_spec = [qual]
            | otherwise         = [qual, unqual]
          where qual = RdrName.Qual (RdrName.is_as decl_spec) occ

lookupRdrNames :: RdrName.GlobalRdrEnv -> Name.Name -> [RdrName.RdrName]
lookupRdrNames gre name = concatMap greToRdrNames $ RdrName.lookupGRE_Name gre name

listAllNames' :: FilePath -> [String] -> GhcMonad.Ghc ([H.HashMap T.Text String], [H.HashMap T.Text String], [H.HashMap T.Text String])
listAllNames' file importStr = GhcMonad.withSession $ \sess -> do
    let df = HscTypes.hsc_dflags sess
    let prelude = if DynFlags.xopt DynFlags.Opt_ImplicitPrelude df
                      then ("import Prelude":)
                      else id
    GHC.setContext . map GHC.IIDecl . rights =<< mapM (parseModule file) (prelude importStr)
    names <- GHC.getNamesInScope
    let (tyCon, other) = span Name.isTyConName   names
        (dCon, func)   = span Name.isDataConName other
    fs <- forM func $ \n -> do
        typf <- typeKind =<< typeOf sess n
        word <- sdocToString (Outputable.ppr n)
        return . H.fromList $ typf [("word", word), ("menu", "[Function]")]
    ts <- forM tyCon $ \n -> do
        word <- sdocToString (Outputable.ppr n)
        return . H.fromList $ [("word", word), ("menu", "[TyCon]")]
    ds <- forM dCon $ \n -> do
        typf <- typeKind =<< typeOf sess n
        word <- sdocToString (Outputable.ppr n)
        return . H.fromList $ typf [("word", word), ("menu", "[DataCon]")]
    return (ts, ds, fs)

nameMenu :: Name.Name -> String
nameMenu n | Name.isDataConName n = "[DataCon]"
           | Name.isTyConName   n = "[TyCon]"
           | otherwise            = "[Function]"

typeKind :: Maybe Type.Type -> GhcMonad.Ghc ([(T.Text, String)] -> [(T.Text, String)])
typeKind Nothing  = return id
typeKind (Just t) = do
    typ <- sdocToString (Type.pprSigmaType t)
    return (("kind", (":: " ++ typ)):)

listAllNames :: C.CString -> IO C.CString
listAllNames arg = do
    (file, xfs, is) <- T.lines . T.decodeUtf8 <$> S.unsafePackCString arg >>= return . \case
        []     -> ("<listAllNames>", "",             [])
        [l]    -> ("<listAllNames>", T.encodeUtf8 l, [])
        [f,l]  -> (T.unpack f,       T.encodeUtf8 l, [])
        f:l:is -> (T.unpack f,       T.encodeUtf8 l, map T.unpack is)
    runGHC file (parseXFlags xfs) (listAllNames' file is) >>= \case
        Nothing -> newCString "[[], []]"
        Just fs -> newCStringFromBS . L.toStrict $ J.encode fs

--------------------------------------------------------------------------------

queryHash' :: T.Text -> GhcMonad.Ghc Int
queryHash' = go . T.lines
  where
    salt        = 0xdc36d1615b7400a4
    xflags x    = foldl' Hash.hashWithSalt salt . sort . map (\(On o) -> fromEnum o) $ filter isOn (parseXFlags x)
    go []       = return $ Hash.hash (0::Int)
    go [x]      = return $ xflags (T.encodeUtf8 x)
    go [f,x]    = return $ Hash.hashWithSalt (xflags $ T.encodeUtf8 x) $ T.strip f
    go (f:x:is) = do
        df  <- GHC.getSessionDynFlags
        pis <- sdocToString . Outputable.ppr =<< rights <$> mapM (readModule df "" . T.unpack) is
        return $ Hash.hashWithSalt (Hash.hashWithSalt (xflags $ T.encodeUtf8 x) $ T.strip f) (sort pis)

queryHash :: C.CString -> IO C.CInt
queryHash cstr = S.unsafePackCString cstr >>= \bs -> 
    maybe 0 fromIntegral <$> runGHC "queryHash" [] (queryHash' $ T.decodeUtf8 bs)

--------------------------------------------------------------------------------

listNamesInModule' :: Module.ModuleName -> GhcMonad.Ghc [H.HashMap T.Text String]
listNamesInModule' moduleName = GhcMonad.withSession $ \sess -> do
    GHC.setContext [HscTypes.IIDecl $ HsImpExp.simpleImportDecl moduleName]
    names <- GHC.getNamesInScope
    forM names $ \n -> do
        typf <- typeKind =<< typeOf sess n
        word <- sdocToString (Outputable.ppr n)
        return . H.fromList $ typf [("word", word), ("menu", nameMenu n)]

listNamesInModule :: C.CString -> IO C.CString
listNamesInModule cstr = C.peekCString cstr >>= \str ->
    runGHC "<listNamesInModule>" [] (listNamesInModule' $ Module.mkModuleName str) >>=
    newCStringFromBS . L.toStrict . J.encode  

--------------------------------------------------------------------------------

data OnOff a = On a | Off a deriving Show

isOn :: OnOff a -> Bool
isOn (On _) = True
isOn _      = False

xFlagParser :: P.Parser (OnOff DynFlags.ExtensionFlag)
xFlagParser = PC.skipSpace *> pragma
  where pragma = P.choice $
            concatMap (\(s,c,_) -> [ Off c <$ P.string (SC.pack $ "No" ++ s)
                                   , On  c <$ P.string (SC.pack s)
                                   ]) DynFlags.xFlags

parseXFlags :: SC.ByteString -> [OnOff DynFlags.ExtensionFlag]
parseXFlags str = rights $ map (P.parseOnly xFlagParser) (SC.split ',' str)

applyXFlags :: [OnOff DynFlags.ExtensionFlag] -> DynFlags.DynFlags -> DynFlags.DynFlags
applyXFlags = flip $ foldl' (\dyn f -> case f of
    On  a -> DynFlags.xopt_set   dyn a
    Off a -> DynFlags.xopt_unset dyn a)

