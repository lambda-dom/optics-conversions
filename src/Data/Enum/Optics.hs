{- |
Module: Data.Enum.Optics

Optics for conversions between enumerations and t'Int'.
-}

module Data.Enum.Optics (
    -- * Prisms.
    enum,
) where

-- Imports.
-- Libraries.
import Optics.Core (Prism', prism')


{- | The prism for conversion of enumerations to 'Int'. -}
{-# INLINEABLE enum #-}
enum :: forall a . (Enum a, Bounded a) => Prism' Int a
enum = prism' construct match
    where
        construct :: a -> Int
        construct = fromEnum

        match :: Int -> Maybe a
        match n =
            if fromEnum (minBound @a) <= n && n <= fromEnum (maxBound @a)
                then Just $ toEnum n
                else Nothing
