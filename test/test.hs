import Test.Tasty
import Test.Tasty.HUnit

import Control.Applicative
import qualified Hassistant as H
import System.FilePath (equalFilePath)
import System.Directory (makeRelativeToCurrentDirectory)

main = defaultMain $ testGroup "Hassistant" [getRoot', getSandboxPackageDB]

newtype FP = FP FilePath
instance Eq FP where
    FP a == FP b = equalFilePath a b
instance Show FP where
    show (FP a) = show a

rootTest :: FilePath -> Maybe FilePath -> TestTree
rootTest dir ex = testCase ("root of " ++ dir ++ " = " ++ show ex) $
    fmap FP <$> H.getRoot' dir >>= assertEqual ("should be " ++ show ex) (FP <$> ex)

getRoot' :: TestTree
getRoot' = testGroup "getRoot'" 
    [ testGroup "test/data/testroot/"
        [ rootTest "test/data/testroot/hoge.hs" (Just "test/data/testroot")
        , rootTest "test/data/testroot/"        (Just "test/data/testroot")
        , rootTest "test/data/testroot"         (Just "test/data/testroot")
        ]
    , testGroup "test/data/testroot/testcabalroot"
        [ rootTest "test/data/testroot/testcabalroot/huga.hs" (Just "test/data/testroot/testcabalroot")
        , rootTest "test/data/testroot/testcabalroot"         (Just "test/data/testroot/testcabalroot")
        ]
    , testGroup "test/data/testroot/testsubroot"
        [ rootTest "test/data/testroot/testsubroot/huga.hs" (Just "test/data/testroot/testsubroot")
        , rootTest "test/data/testroot/testsubroot"         (Just "test/data/testroot/testsubroot")
        ]
    , testGroup "test/data/testroot/testsubdir"
        [ rootTest "test/data/testroot/testsubdir/huga.hs" (Just "test/data/testroot")
        , rootTest "test/data/testroot/testsubdir"         (Just "test/data/testroot")
        ]
    ]

getSandboxPackageDB :: TestTree
getSandboxPackageDB = testGroup "getSandboxPackageDB"
    [ testCase "correct cabal.sandbox.config" $ H.getSandboxPackageDB "test/data/testroot/cabal.sandbox.config" >>=
        assertEqual "" (Just "/Users/philopon/.vim/bundle/hassistant.vim/test/data/testroot/.cabal-sandbox/x86_64-osx-ghc-7.6.3-packages.conf.d")
    , testCase "broken cabal.sandbox.config" $ H.getSandboxPackageDB "test/data/testroot/testsubroot/cabal.sandbox.config" >>=
        assertEqual "" Nothing
    ]


