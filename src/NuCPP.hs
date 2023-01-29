module NuCPP (main, nucpp) where

import System.Environment (getArgs)
import System.Exit        (exitFailure)
import System.IO          (hPutStrLn, stderr)

import NuCPP.Args
import NuCPP.Imports

import qualified Data.Map.Strict             as Map
import qualified Language.Preprocessor.Cpphs as C

main :: IO ()
main = do
    args0 <- getArgs
    -- hPutStrLn stderr $ "cli args: " ++ show args0
    args <- either fail return (parseArgs args0)
    -- hPutStrLn stderr $ "parsed args: " ++ show args
    cpphs args

-- | Run nucpp with given arguments.
nucpp :: Args -> IO ()
nucpp = cpphs

cpphs :: Args -> IO ()
cpphs args = do
    input <- case argsInput args of
        Nothing -> getContents
        Just fp -> readFile fp

    output <- C.runCpphs cpphsOpts (fromMaybe "<stdin>" (argsInput args)) input

    case argsOutput args of
        Nothing -> putStr output
        Just fp -> writeFile fp output
  where
    cpphsOpts :: C.CpphsOptions
    cpphsOpts = C.CpphsOptions
        { C.infiles    = []
        , C.outfiles   = []
        , C.defines    = Map.toList (argsDefines args)
        , C.includes   = toList (optsIncludeDirs (argsOpts args))
        , C.preInclude = toList (argsIncludes args)
        , C.boolopts   = C.defaultBoolOptions
            { C.macros    = True
            , C.locations = False
            , C.stripEol  = False
            , C.stripC89  = True -- TODO: for cabal_macros.h comments
            }
        }
