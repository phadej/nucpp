#undef MEGAPARSEC_7_OR_LATER
#ifdef MIN_VERSION_GLASGOW_HASKELL
-- GHC >= 7.10.1.0
#if MIN_VERSION_GLASGOW_HASKELL(8,0,0,0)
-- GHC >= 8.0.0.0
#if MIN_VERSION_megaparsec(7,0,0)
#define MEGAPARSEC_7_OR_LATER
#endif
#endif
#endif