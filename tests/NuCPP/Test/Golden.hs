module NuCPP.Test.Golden (tests) where

import System.FilePath   ((-<.>), (</>))
import Test.Tasty        (TestName, TestTree, testGroup)
import Test.Tasty.Golden (goldenVsFileDiff)

import NuCPP      (nucpp)
import NuCPP.Args (parseArgs)

tests :: TestTree
tests = testGroup "golden"
    [ goldenTest "singletons-bool-01" []
    , goldenTest "singletons-bool-02" ["-D__GLASGOW_HASKELL__=900"]
    , goldenTest "cabal-macros-01"    []
    , goldenTest "multiline-01"       []

    -- This doesn't work:
    -- https://github.com/malcolmwallace/cpphs/issues/26
    -- , goldenTest "ghcversion-01.i" ["-include","fixtures/ghcversion.h","-include","fixtures/ghc_tmp.h"]
    ]
  where
    --  TODO: use VsString
    goldenTest :: TestName -> [String] -> TestTree
    goldenTest name args0 = goldenVsFileDiff name diff golden output action where
        action = do
            args <- either fail return $ parseArgs $ args0 ++ [input, output]
            nucpp args

        input  = "fixtures" </> name -<.> "i"
        golden = "fixtures" </> name -<.> "o"
        output = "fixtures" </> name -<.> "tmp"

        diff ref new = ["diff", "-u", ref, new]
