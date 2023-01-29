{-# LANGUAGE CPP #-}
-- | Command line arguments
module NuCPP.Args (
    Opts (..),
    Args (..),
    parseArgs,
) where

import NuCPP.Imports
import NuCPP.Optics

import qualified Data.Map.Strict as Map

#define MAKEOPTIC(field) Optic $ \f s -> f (field s) <&> \x -> s { field = x }

-------------------------------------------------------------------------------
-- Opts
-------------------------------------------------------------------------------

-- | Options: how to process input.
data Opts = Opts
    { optsIncludeDirs :: SnocList FilePath  -- ^ include directories, used to lookup @#include@ files.
    -- TODO: noline, hsline, cline
    -- TODO: nocontinuation definecontinuation linecontinuations
    -- TODO: // comments, everywhere or imports only
    -- TODO: /* comments */
    -- TODO: ddump-*
    , optsWerror :: Bool  -- ^ whether warnings are fatal. TODO: currently unused
    }
  deriving (Eq, Show)

emptyOpts :: Opts
emptyOpts = Opts
    { optsIncludeDirs = mempty
    , optsWerror      = False
    }

instance (a ~ b, a ~ SnocList FilePath) => LabelOptic "includeDirs" Functor Opts Opts a b where
    labelOptic _ = MAKEOPTIC(optsIncludeDirs)

-------------------------------------------------------------------------------
-- Args
-------------------------------------------------------------------------------

-- | Arguments consist of
--
-- * 'Opts', options how to process input
-- * the input: defines, includes and input file
-- * the output file
--
data Args = Args
    { argsOpts     :: Opts               -- ^ options

 -- TODO: constant, values, move to args
    , argsDefines  :: Map String String  -- ^ defines, processed first.
    , argsIncludes :: SnocList FilePath  -- ^ included files, processed second.
    , argsInput    :: Maybe FilePath     -- ^ input file, processed last. 'stdin' on 'Nothing'.
    , argsOutput   :: Maybe FilePath     -- ^ output file. 'stdout' on 'Nothing'.
    -- TODO: argsVersion
    -- TODO: argsHelp
    -- TODO: argsMan
    }
  deriving (Eq, Show)

emptyArgs :: Args
emptyArgs = Args
    { argsOpts     = emptyOpts
    , argsDefines  = mempty
    , argsIncludes = mempty
    , argsInput    = Nothing
    , argsOutput   = Nothing
    }

instance (a ~ b, a ~ Opts) => LabelOptic "opts" Functor Args Args a b where
    labelOptic _ = MAKEOPTIC(argsOpts)

instance (a ~ b, a ~ Map String String) => LabelOptic "defines" Functor Args Args a b where
    labelOptic _ = MAKEOPTIC(argsDefines)

instance (a ~ b, a ~ SnocList FilePath) => LabelOptic "includes" Functor Args Args a b where
    labelOptic _ = MAKEOPTIC(argsIncludes)

-------------------------------------------------------------------------------
-- Define: TODO move to own module.
-------------------------------------------------------------------------------

parseDefine :: String -> (String, String)
parseDefine str = case sfx of
    '=' : sfx' -> (pfx, sfx')
    _          -> (str, "1")
  where
    (pfx, sfx) = span (/= '=') str

-------------------------------------------------------------------------------
-- Input & Output
-------------------------------------------------------------------------------

data InputOutput
    = NoInputOutput
    | InputSet (Maybe FilePath)
    | OutputSet (Maybe FilePath)
    | InputOutputSet (Maybe FilePath) (Maybe FilePath)

input_ :: InputOutput -> Maybe FilePath
input_ NoInputOutput        = Nothing
input_ (InputSet x)         = x
input_ (OutputSet _)        = Nothing
input_ (InputOutputSet x _) = x

output_ :: InputOutput -> Maybe FilePath
output_ NoInputOutput        = Nothing
output_ (InputSet _)         = Nothing
output_ (OutputSet y)        = y
output_ (InputOutputSet _ y) = y

-------------------------------------------------------------------------------
-- Argument parsing
-------------------------------------------------------------------------------

-- | Parse arguments
--
-- The argument parsing is implemented manually as it's a bit intricate.
-- "System.Console.GetOpt" couldn't handle single dash long parameter @-include@ for example.
--
parseArgs :: [String] -> Either String Args
parseArgs = go NoInputOutput emptyArgs where
    go :: InputOutput -> Args -> [String] -> Either String Args
    go io acc []                   = return acc { argsInput = input_ io, argsOutput = output_ io }
    go io acc (('-' : arg) : args) = parseArg io acc arg args
    go io acc (arg         : args) = do
        io' <- setInput io arg
        go io' acc args

    parseArg :: InputOutput -> Args -> String -> [String] -> Either String Args
    -- -include: include files
    parseArg _  _   "include"     []          = Left "Missing path after '-include'"
    parseArg io acc "include"     (fp : args) = go io (addInclude fp acc) args
    parseArg io acc ('i' : 'n' : 'c' : 'l' : 'u' : 'd' : 'e' : fp)
                                  args        = go io (addInclude fp acc) args

    -- -D: defines
    -- TODO: accept -D foo
    parseArg io acc ('D' : arg)   args        = go io (addDefine arg acc) args

    -- -I: include dirs
    parseArg _  _   "I"           []          = Left "Missing path after '-I'"
    parseArg io acc "I"           (fp : args) = go io (addIncludeDir fp acc) args
    parseArg io acc ('I' : fp)    args        = go io (addIncludeDir fp acc) args

    -- -o: output file
    parseArg _  _   "o"           []          = Left "Missing path after '-o'"
    parseArg io acc "o"           (fp : args) = do
        io' <- setOutput io fp
        go io' acc args
    parseArg io acc ('o' : fp)    args        = do
        io' <- setOutput io fp
        go io' acc args

    -- -x: ignored
    parseArg _  _   "x"           []          = Left "Missing path after '-x'"
    parseArg io acc "x"           (_  : args) = go io acc args
    parseArg io acc ('x' : _)     args        = go io acc args

    -- -undef, -traditional, -Werror: ignored
    parseArg io acc "undef"       args        = go io acc args
    parseArg io acc "traditional" args        = go io acc args
    parseArg io acc "Werror"      args        = go io acc args

    parseArg _  _   arg           _args       = Left $ "Unknown argument: " ++ show arg

    -- Setting input and output, either can be set only once
    setInput :: InputOutput -> FilePath -> Either String InputOutput
    setInput NoInputOutput        fp = return $ InputSet (parseInputOutput fp)
    setInput (InputSet i)         fp = return $ InputOutputSet i (parseInputOutput fp)
    setInput (OutputSet o)        fp = return $ InputOutputSet (parseInputOutput fp) o
    setInput (InputOutputSet _ _) _  = Left "output filename specified twice"

    setOutput :: InputOutput -> FilePath -> Either String InputOutput
    setOutput NoInputOutput        fp = return $ OutputSet (parseInputOutput fp)
    setOutput (InputSet i)         fp = return $ InputOutputSet i (parseInputOutput fp)
    setOutput (OutputSet _)        _  = Left "output filename specified twice"
    setOutput (InputOutputSet _ _) _  = Left "output filename specified twice"

    parseInputOutput :: FilePath -> Maybe FilePath
    parseInputOutput "-" = Nothing
    parseInputOutput fp  = Just fp

    -- args and option modifiers
    addInclude :: FilePath -> Args -> Args
    addInclude fp = over #includes (:> fp)

    addDefine :: String -> Args -> Args
    addDefine d args = case parseDefine d of
        (name, define) -> over #defines (Map.insert name define) args

    addIncludeDir :: FilePath -> Args -> Args
    addIncludeDir fp = over (#opts % #includeDirs) (:> fp)
