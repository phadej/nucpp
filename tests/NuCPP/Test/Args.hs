{-# OPTIONS_GHC -Wno-orphans #-}
module NuCPP.Test.Args (tests) where

import Data.Foldable              (toList)
import Data.TreeDiff.Class        (ToExpr (toExpr))
import Data.TreeDiff.Golden       (ediffGolden)
import GHC.Generics               (Generic)
import Test.Tasty                 (TestTree, testGroup)
import Test.Tasty.Golden.Advanced (goldenTest)

import NuCPP.Args
import NuCPP.SnocList (SnocList)

tests :: TestTree
tests = testGroup "args"
    [ ediffGolden goldenTest "example01" "fixtures/example01.expr" $
      either fail return $ parseArgs example01
    ]

-- example compiling splitmix
example01 :: [String]
example01 =
    [ "-DSPLITMIX_INIT_C=1"
    , "-include"
    , "/code/public-haskell/splitmix/dist-newstyle/build/x86_64-linux/ghc-8.6.5/splitmix-0.1.0.4/build/autogen/cabal_macros.h"
    , "-I/code/public-haskell/splitmix/dist-newstyle/build/x86_64-linux/ghc-8.6.5/splitmix-0.1.0.4/build"
    , "-I/code/public-haskell/splitmix/dist-newstyle/build/x86_64-linux/ghc-8.6.5/splitmix-0.1.0.4/build"
    , "-I/code/public-haskell/splitmix/dist-newstyle/build/x86_64-linux/ghc-8.6.5/splitmix-0.1.0.4/build/autogen"
    , "-I/code/public-haskell/splitmix/dist-newstyle/build/x86_64-linux/ghc-8.6.5/splitmix-0.1.0.4/build/global-autogen"
    , "-I/code/public-haskell/splitmix/dist-newstyle/build/x86_64-linux/ghc-8.6.5/splitmix-0.1.0.4/build"
    , "-I/opt/ghc/8.6.5/lib/ghc-8.6.5/base-4.12.0.0/include"
    , "-I/opt/ghc/8.6.5/lib/ghc-8.6.5/integer-gmp-1.0.2.0/include"
    , "-I/opt/ghc/8.6.5/lib/ghc-8.6.5/include"
    , "-include"
    , "/opt/ghc/8.6.5/lib/ghc-8.6.5/include/ghcversion.h"
    , "-Dlinux_BUILD_OS"
    , "-Dx86_64_BUILD_ARCH"
    , "-Dlinux_HOST_OS"
    , "-Dx86_64_HOST_ARCH"
    , "-D__GLASGOW_HASKELL_TH__"
    , "-D__SSE__"
    , "-D__SSE2__"
    , "-include/tmp/ghc51967_0/ghc_2.h"
    , "-x"
    , "assembler-with-cpp"
    , "src/System/Random/SplitMix.hs"
    , "-o"
    , "/tmp/ghc51967_0/ghc_1.hscpp"
    ]

-------------------------------------------------------------------------------
-- orphans
-------------------------------------------------------------------------------

deriving instance Generic Opts
deriving instance Generic Args

instance ToExpr Opts
instance ToExpr Args

instance ToExpr a => ToExpr (SnocList a) where
    toExpr = toExpr . toList
