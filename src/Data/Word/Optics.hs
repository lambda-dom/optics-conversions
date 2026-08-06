{- |
Module: Data.Word.Optics

Optics for conversions from 'Word' and its fixed size variants to other integral types.
-}

module Data.Word.Optics (
    -- * Isomorphisms for lossless conversions between integral, primitive types.
    word8ToInt8,
    word16ToInt16,
    word32ToInt32,
    word64ToInt64,
    wordToInt,

    -- * Prisms for conversion between integral, primitive types.
    word8ToWord16,
    word8ToWord32,
    word8ToWord64,
    word8ToWord,
) where

-- Imports.
-- Base.
import Data.Bits (toIntegralSized)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)

-- Libraries.
import Optics.Core (Prism', Iso', prism', iso)


{- | Isomorphism for conversion between 'Word8' and 'Int8'.

note(s):

    * Uses 'fromIntegral' for both directions.
-}
{-# INLINEABLE word8ToInt8 #-}
word8ToInt8 :: Iso' Word8 Int8
word8ToInt8 = iso fromIntegral fromIntegral

{- | Isomorphism for conversion between 'Word16' and 'Int16'.

note(s):

    * Uses 'fromIntegral' for both directions.
-}
{-# INLINEABLE word16ToInt16 #-}
word16ToInt16 :: Iso' Word16 Int16
word16ToInt16 = iso fromIntegral fromIntegral

{- | Isomorphism for conversion between 'Word32' and 'Int32'.

note(s):

    * Uses 'fromIntegral' for both directions.
-}
{-# INLINEABLE word32ToInt32 #-}
word32ToInt32 :: Iso' Word32 Int32
word32ToInt32 = iso fromIntegral fromIntegral

{- | Isomorphism for conversion between 'Word64' and 'Int64'.

note(s):

    * Uses 'fromIntegral' for both directions.
-}
{-# INLINEABLE word64ToInt64 #-}
word64ToInt64 :: Iso' Word64 Int64
word64ToInt64 = iso fromIntegral fromIntegral

{- | Isomorphism for conversion between 'Word' and 'Int'.

note(s):

    * Uses 'fromIntegral' for both directions.
-}
{-# INLINEABLE wordToInt #-}
wordToInt :: Iso' Word Int
wordToInt = iso fromIntegral fromIntegral


{- | Prism for conversion between 'Word8' and 'Word16'.

note(s):

    * Uses 'fromIntegral' for the lossless conversion and 'toIntegralSized' for the other direction.
-}
{-# INLINEABLE word8ToWord16 #-}
word8ToWord16 :: Prism' Word16 Word8
word8ToWord16 = prism' fromIntegral toIntegralSized

{- | Prism for conversion between 'Word8' and 'Word32'.

note(s):

    * Uses 'fromIntegral' for the lossless conversion and 'toIntegralSized' for the other direction.
-}
{-# INLINEABLE word8ToWord32 #-}
word8ToWord32 :: Prism' Word32 Word8
word8ToWord32 = prism' fromIntegral toIntegralSized

{- | Prism for conversion between 'Word8' and 'Word64'.

note(s):

    * Uses 'fromIntegral' for the lossless conversion and 'toIntegralSized' for the other direction.
-}
{-# INLINEABLE word8ToWord64 #-}
word8ToWord64 :: Prism' Word64 Word8
word8ToWord64 = prism' fromIntegral toIntegralSized

{- | Prism for conversion between 'Word8' and 'Word64'.

note(s):

    * Uses 'fromIntegral' for the lossless conversion and 'toIntegralSized' for the other direction.
-}
{-# INLINEABLE word8ToWord #-}
word8ToWord :: Prism' Word Word8
word8ToWord = prism' fromIntegral toIntegralSized
