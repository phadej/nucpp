module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import qualified NuCPP.Test.Args
import qualified NuCPP.Test.Golden

main :: IO ()
main = defaultMain $ testGroup "nucpp"
    [ NuCPP.Test.Args.tests
    , NuCPP.Test.Golden.tests
    ]
