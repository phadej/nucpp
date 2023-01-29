# if __GLASGOW_HASKELL__ >= 806
#  define KVS(kvs) kvs
# else
#  define KVS(kvs)
# endif

-- | Useful combination of 'sbool' and 'eqToRefl'
--
-- @since 0.1.2.0
sboolEqRefl :: forall KVS(k) (a :: k) (b :: k). SBoolI (a == b) => Maybe (a :~: b)
