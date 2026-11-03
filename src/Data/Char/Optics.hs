{- |
Module: Data.Char.Optics

Optics for conversions involving t'Char'.
-}

module Data.Char.Optics (
    -- * Prisms.
    category,
    ascii,
) where

-- Imports.
-- Base.
import Data.Char (chr, ord, isAscii)

-- Libraries.
import Optics.Core (Prism', prism')


-- $setup
-- >>> import Optics.Core


{- | Prism for converting between a subcategory of utf-8 characters and 'Int'. -}
{-# INLINE category #-}
category :: (Char -> Bool) -> Prism' Int Char
category p = prism' construct match
    where
        construct :: Char -> Int
        construct = ord

        match :: Int -> Maybe Char
        match n = let c = chr n in if p c then Just c else Nothing

{- | Prism for converting between ascii characters and 'Int'.

Defined as:

@ascii = category isAscii@

=== __Examples:__

>>> review ascii '\n'
10

>>> preview ascii 10
Just '\n'

>>> preview ascii 255
Nothing
-}
{-# INLINE ascii #-}
ascii :: Prism' Int Char
ascii = category isAscii
