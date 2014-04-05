import Control.Arrow(second)
import Distribution.Simple
import Distribution.PackageDescription
import Distribution.Text (display)
import qualified Distribution.Simple.LocalBuildInfo as LBI
import qualified Distribution.Simple.Compiler       as Compiler
import qualified Distribution.Simple.Configure      as Configure

main :: IO ()
main = defaultMainWithHooks simpleUserHooks { confHook = myConfHook }

myConfHook gpdhbi flg = do
    lbi <- Configure.configure gpdhbi flg
    let compiler     = showCompiler $ LBI.compiler lbi
        pkgDescr     = LBI.localPkgDescr lbi
        ([so], exes) = span ((== "library.so") . exeName) (executables pkgDescr)
        opts         = map (second (("-lHSrts-" ++ compiler):)) . options $ buildInfo so
        so'          = so { buildInfo = (buildInfo so) { options = opts } }
    return lbi { LBI.localPkgDescr = pkgDescr {executables = so':exes }}

showCompiler c =
    let Compiler.CompilerId f v = Compiler.compilerId c
    in display f ++ display v
