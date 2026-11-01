{- |
Module: Data.Char.Optics

Optics for conversions involving t'Char'.
-}

module Data.Char.Optics (
    -- * Prisms.
    ascii,
) where

-- Imports.
-- Base.
import Data.Char (chr, ord)
import Data.Word (Word8)

-- Libraries.
import Optics.Core (Prism', prism')


-- $setup
-- >>> import Optics.Core


{- | Prism for converting between ascii characters and 'Word8'.

=== __Examples:__

>>> review ascii '\n'
10

>>> preview ascii 10
Just '\n'

>>> preview ascii 255
Nothing
-}
{-# INLINE ascii #-}
ascii :: Prism' Word8 Char
ascii = prism' construct match
    where
        construct :: Char -> Word8
        construct = fromIntegral @Int @Word8 . ord

        match :: Word8 -> Maybe Char
        match n = if n <= 127 then Just . chr $ fromIntegral @Word8 @Int n else Nothing
