-- | Small lens/optics like library with OverloadedLabels support
module NuCPP.Optics where

import GHC.OverloadedLabels (IsLabel (..))

import NuCPP.Imports

newtype Optic f s t a b = Optic { unOptic :: (a -> f b) -> (s -> f t) }

-- | composition
(%) :: Optic f s t a b -> Optic f a b u v -> Optic f s t u v
Optic f % Optic g = Optic (f . g)

-- | Get value using optic.
view :: Optic (Const a) s s a a -> s -> a
view o s = getConst (unOptic o Const s)

-- | Modify value using optic.
over :: Optic Identity s t a b -> (a -> b) -> (s -> t)
over = coerce

-- | Put value using optic.
set :: Optic Identity s t a b -> b -> s -> t
set o b = over o (const b)

-------------------------------------------------------------------------------
-- Labels
-------------------------------------------------------------------------------

instance (LabelOptic x c s t a b, c f) => IsLabel x (Optic f s t a b) where
    fromLabel = labelOptic @x @c @s @t @a @b Proxy

class LabelOptic (x :: Symbol) c s t a b | x s -> c, s -> a, t -> b, s b -> t, t a -> s where
    labelOptic :: c f => Proxy x -> Optic f s t a b
