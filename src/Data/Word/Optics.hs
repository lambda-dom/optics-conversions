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
    word64ToWord,

    -- * Prisms for conversion between integral, unsigned primitive types.
    word8ToWord16,
    word8ToWord32,
    word8ToWord64,
    word8ToWord,
    word8ToInt16,
    word8ToInt32,
    word8ToInt64,
    word8ToInt,
    word8ToInteger,
    word16ToWord32,
    word16ToWord64,
    word16ToWord,
    word16ToInt32,
    word16ToInt64,
    word16ToInt,
    word16ToInteger,
    word32ToWord64,
    word32ToWord,
    word32ToInt64,
    word32ToInt,
    word32ToInteger,
    word64ToInteger,
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
{-# INLINE word8ToInt8 #-}
word8ToInt8 :: Iso' Word8 Int8
word8ToInt8 = iso fromIntegral fromIntegral

{- | Isomorphism for conversion between 'Word16' and 'Int16'.

note(s):

    * Uses 'fromIntegral' for both directions.
-}
{-# INLINE word16ToInt16 #-}
word16ToInt16 :: Iso' Word16 Int16
word16ToInt16 = iso fromIntegral fromIntegral

{- | Isomorphism for conversion between 'Word32' and 'Int32'.

note(s):

    * Uses 'fromIntegral' for both directions.
-}
{-# INLINE word32ToInt32 #-}
word32ToInt32 :: Iso' Word32 Int32
word32ToInt32 = iso fromIntegral fromIntegral

{- | Isomorphism for conversion between 'Word64' and 'Int64'.

note(s):

    * Uses 'fromIntegral' for both directions.
-}
{-# INLINE word64ToInt64 #-}
word64ToInt64 :: Iso' Word64 Int64
word64ToInt64 = iso fromIntegral fromIntegral

{- | Isomorphism for conversion between 'Word' and 'Int'.

note(s):

    * Uses 'fromIntegral' for both directions.
-}
{-# INLINE wordToInt #-}
wordToInt :: Iso' Word Int
wordToInt = iso fromIntegral fromIntegral

{- | Isomorphism for conversion between 'Word64' and 'Word'.

note(s):

    * The isomorphism is only valid if 'Word' has 64 bits. This is the common case, but is not
    guaranteed by the Haskell report.

    * Uses 'fromIntegral' for both directions.
-}
{-# INLINE word64ToWord #-}
word64ToWord :: Iso' Word64 Word
word64ToWord = iso fromIntegral fromIntegral


{- | Prism for conversion between 'Word8' and 'Word16'.

note(s):

    * Uses 'fromIntegral' for the lossless conversion and 'toIntegralSized' for the other direction.
-}
{-# INLINE word8ToWord16 #-}
word8ToWord16 :: Prism' Word16 Word8
word8ToWord16 = prism' fromIntegral toIntegralSized

{- | Prism for conversion between 'Word8' and 'Word32'.

note(s):

    * Uses 'fromIntegral' for the lossless conversion and 'toIntegralSized' for the other direction.
-}
{-# INLINE word8ToWord32 #-}
word8ToWord32 :: Prism' Word32 Word8
word8ToWord32 = prism' fromIntegral toIntegralSized

{- | Prism for conversion between 'Word8' and 'Word64'.

note(s):

    * Uses 'fromIntegral' for the lossless conversion and 'toIntegralSized' for the other direction.
-}
{-# INLINE word8ToWord64 #-}
word8ToWord64 :: Prism' Word64 Word8
word8ToWord64 = prism' fromIntegral toIntegralSized

{- | Prism for conversion between 'Word8' and 'Word'.

note(s):

    * Uses 'fromIntegral' for the lossless conversion and 'toIntegralSized' for the other direction.
-}
{-# INLINE word8ToWord #-}
word8ToWord :: Prism' Word Word8
word8ToWord = prism' fromIntegral toIntegralSized

{- | Prism for conversion between 'Word8' and 'Int16'.

note(s):

    * Uses 'fromIntegral' for the lossless conversion and 'toIntegralSized' for the other direction.
-}
{-# INLINE word8ToInt16 #-}
word8ToInt16 :: Prism' Int16 Word8
word8ToInt16 = prism' fromIntegral toIntegralSized

{- | Prism for conversion between 'Word8' and 'Int32'.

note(s):

    * Uses 'fromIntegral' for the lossless conversion and 'toIntegralSized' for the other direction.
-}
{-# INLINE word8ToInt32 #-}
word8ToInt32 :: Prism' Int32 Word8
word8ToInt32 = prism' fromIntegral toIntegralSized

{- | Prism for conversion between 'Word8' and 'Int64'.

note(s):

    * Uses 'fromIntegral' for the lossless conversion and 'toIntegralSized' for the other direction.
-}
{-# INLINE word8ToInt64 #-}
word8ToInt64 :: Prism' Int64 Word8
word8ToInt64 = prism' fromIntegral toIntegralSized

{- | Prism for conversion between 'Word8' and 'Int'.

note(s):

    * Uses 'fromIntegral' for the lossless conversion and 'toIntegralSized' for the other direction.
-}
{-# INLINE word8ToInt #-}
word8ToInt :: Prism' Int Word8
word8ToInt = prism' fromIntegral toIntegralSized

{- | Prism for conversion between 'Word8' and 'Integer'.

note(s):

    * Uses 'fromIntegral' for the lossless conversion and 'toIntegralSized' for the other direction.
-}
{-# INLINE word8ToInteger #-}
word8ToInteger :: Prism' Integer Word8
word8ToInteger = prism' fromIntegral toIntegralSized

{- | Prism for conversion between 'Word16' and 'Word32'.

note(s):

    * Uses 'fromIntegral' for the lossless conversion and 'toIntegralSized' for the other direction.
-}
{-# INLINE word16ToWord32 #-}
word16ToWord32 :: Prism' Word32 Word16
word16ToWord32 = prism' fromIntegral toIntegralSized

{- | Prism for conversion between 'Word16' and 'Word64'.

note(s):

    * Uses 'fromIntegral' for the lossless conversion and 'toIntegralSized' for the other direction.
-}
{-# INLINE word16ToWord64 #-}
word16ToWord64 :: Prism' Word64 Word16
word16ToWord64 = prism' fromIntegral toIntegralSized

{- | Prism for conversion between 'Word16' and 'Word'.

note(s):

    * Uses 'fromIntegral' for the lossless conversion and 'toIntegralSized' for the other direction.
-}
{-# INLINE word16ToWord #-}
word16ToWord :: Prism' Word Word16
word16ToWord = prism' fromIntegral toIntegralSized

{- | Prism for conversion between 'Word16' and 'Int32'.

note(s):

    * Uses 'fromIntegral' for the lossless conversion and 'toIntegralSized' for the other direction.
-}
{-# INLINE word16ToInt32 #-}
word16ToInt32 :: Prism' Int32 Word16
word16ToInt32 = prism' fromIntegral toIntegralSized

{- | Prism for conversion between 'Word16' and 'Int64'.

note(s):

    * Uses 'fromIntegral' for the lossless conversion and 'toIntegralSized' for the other direction.
-}
{-# INLINE word16ToInt64 #-}
word16ToInt64 :: Prism' Int64 Word16
word16ToInt64 = prism' fromIntegral toIntegralSized

{- | Prism for conversion between 'Word16' and 'Int'.

note(s):

    * Uses 'fromIntegral' for the lossless conversion and 'toIntegralSized' for the other direction.
-}
{-# INLINE word16ToInt #-}
word16ToInt :: Prism' Int Word16
word16ToInt = prism' fromIntegral toIntegralSized

{- | Prism for conversion between 'Word16' and 'Integer'.

note(s):

    * Uses 'fromIntegral' for the lossless conversion and 'toIntegralSized' for the other direction.
-}
{-# INLINE word16ToInteger #-}
word16ToInteger :: Prism' Integer Word16
word16ToInteger = prism' fromIntegral toIntegralSized

{- | Prism for conversion between 'Word32' and 'Word64'.

note(s):

    * Uses 'fromIntegral' for the lossless conversion and 'toIntegralSized' for the other direction.
-}
{-# INLINE word32ToWord64 #-}
word32ToWord64 :: Prism' Word64 Word32
word32ToWord64 = prism' fromIntegral toIntegralSized

{- | Prism for conversion between 'Word32' and 'Word'.

note(s):

    * This prism is only valid if 'Word' has 32 bits or more. This is the common case, but is not
    guaranteed by the Haskell report.

    * Uses 'fromIntegral' for the lossless conversion and 'toIntegralSized' for the other direction.
-}
{-# INLINE word32ToWord #-}
word32ToWord :: Prism' Word Word32
word32ToWord = prism' fromIntegral toIntegralSized

{- | Prism for conversion between 'Word32' and 'Int64'.

note(s):

    * Uses 'fromIntegral' for the lossless conversion and 'toIntegralSized' for the other direction.
-}
{-# INLINE word32ToInt64 #-}
word32ToInt64 :: Prism' Int64 Word32
word32ToInt64 = prism' fromIntegral toIntegralSized

{- | Prism for conversion between 'Word32' and 'Int'.

note(s):

    * This prism is valid only of 'Int' has at least 32 bits, something that is not guaranteed by
    the Haskell report.

    * Uses 'fromIntegral' for the lossless conversion and 'toIntegralSized' for the other direction.
-}
{-# INLINE word32ToInt #-}
word32ToInt :: Prism' Int Word32
word32ToInt = prism' fromIntegral toIntegralSized

{- | Prism for conversion between 'Word32' and 'Integer'.

note(s):

    * Uses 'fromIntegral' for the lossless conversion and 'toIntegralSized' for the other direction.
-}
{-# INLINE word32ToInteger #-}
word32ToInteger :: Prism' Integer Word32
word32ToInteger = prism' fromIntegral toIntegralSized

{- | Prism for conversion between 'Word64' and 'Integer'.

note(s):

    * Uses 'fromIntegral' for the lossless conversion and 'toIntegralSized' for the other direction.
-}
{-# INLINE word64ToInteger #-}
word64ToInteger :: Prism' Integer Word64
word64ToInteger = prism' fromIntegral toIntegralSized
