{-# LANGUAGE NoImplicitPrelude #-}
module NuCPP.SnocList (
    SnocList ((:>)),
    toList,
    snoc,
) where

import Data.Monoid    (Monoid (..))
import Data.Semigroup (Semigroup (..))
import Prelude        (Eq, Maybe (..), Show)

import qualified Data.Foldable as F
import qualified Data.List     as L

newtype SnocList a = SnocList [a]
  deriving (Eq, Show)

instance F.Foldable SnocList where
    toList = toList
    foldMap f (SnocList xs) = F.foldMap f (L.reverse xs)

instance Semigroup (SnocList a) where
    SnocList xs <> SnocList ys = SnocList (ys <> xs)

instance Monoid (SnocList a) where
    mempty = SnocList []
    mappend = (<>)

toList :: SnocList a -> [a]
toList (SnocList xs) = L.reverse xs

snoc :: SnocList a -> a -> SnocList a
snoc (SnocList xs) x = SnocList (x : xs)

unsnoc :: SnocList a -> Maybe (SnocList a, a)
unsnoc (SnocList [])       = Nothing
unsnoc (SnocList (x : xs)) = Just (SnocList xs, x)

pattern (:>) :: SnocList a -> a -> SnocList a
pattern xs :> x <- (unsnoc -> Just (xs, x))
  where xs :> x = snoc xs x
