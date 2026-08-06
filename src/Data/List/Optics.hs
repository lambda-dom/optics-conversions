{- |
Module: Data.List.Optics

Optics for conversions from and to lists.
-}

module Data.List.Optics (
    -- * Prisms.
    nonEmptyToList,
) where

-- Imports.
-- Base.
import Data.List.NonEmpty (NonEmpty, toList, nonEmpty)

-- Libraries.
import Optics.Core (Prism', prism')


{- | Prism for conversion between @'NonEmpty' a@ and @[a]@.

note(s):

    * Uses 'toList' for the lossless conversion and 'nonEmpty' for the other direction.
-}
{-# INLINE nonEmptyToList #-}
nonEmptyToList :: Prism' [a] (NonEmpty a)
nonEmptyToList = prism' toList nonEmpty
