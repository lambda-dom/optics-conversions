{- |
Module: Data.Int.Optics

Optics for conversions from 'Int' and its fixed size variants to other integral types.
-}

module Data.Int.Optics (
    -- * Isomorphisms for lossless conversions between integral, primitive types.
    int8ToWord8,
    int16ToWord16,
    int32ToWord32,
    int64ToWord64,
    int64ToInt,
    intToWord,

    -- * Prisms for conversion between integral, signed primitive types.
    int8ToInt16,
    int8ToInt32,
    int8ToInt64,
    int8ToInt,
    int16ToInt32,
    int16ToInt64,
    int16ToInt,
    int32ToInt64,
    int32ToInt,
) where

-- Imports.
-- Base.
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)

-- Libraries.
import Optics.Core (Iso', Prism', iso, prism')
import Data.Bits (toIntegralSized)


{- | Isomorphism for conversion between 'Int8' and 'Word8'.

note(s):

    * Uses 'fromIntegral' for both directions.
-}
{-# INLINE int8ToWord8 #-}
int8ToWord8 :: Iso' Int8 Word8
int8ToWord8 = iso fromIntegral fromIntegral

{- | Isomorphism for conversion between 'Int16' and 'Word16'.

note(s):

    * Uses 'fromIntegral' for both directions.
-}
{-# INLINE int16ToWord16 #-}
int16ToWord16 :: Iso' Int16 Word16
int16ToWord16 = iso fromIntegral fromIntegral

{- | Isomorphism for conversion between 'Int32' and 'Word32'.

note(s):

    * Uses 'fromIntegral' for both directions.
-}
{-# INLINE int32ToWord32 #-}
int32ToWord32 :: Iso' Int32 Word32
int32ToWord32 = iso fromIntegral fromIntegral

{- | Isomorphism for conversion between 'Int64' and 'Word64'.

note(s):

    * Uses 'fromIntegral' for both directions.
-}
{-# INLINE int64ToWord64 #-}
int64ToWord64 :: Iso' Int64 Word64
int64ToWord64 = iso fromIntegral fromIntegral

{- | Isomorphism for conversion between 'Int64' and 'Int'.

note(s):

    * This isomorphism is only valid in platforms where 'Int' has 64 bits.
    * Uses 'fromIntegral' for both directions.
-}
{-# INLINE int64ToInt #-}
int64ToInt :: Iso' Int64 Int
int64ToInt = iso fromIntegral fromIntegral

{- | Isomorphism for conversion between 'Int' and 'Word'.

note(s):

    * Uses 'fromIntegral' for both directions.
-}
{-# INLINE intToWord #-}
intToWord :: Iso' Int Word
intToWord = iso fromIntegral fromIntegral


{- | Prism for conversion between 'Int8' and 'Int16'.

note(s):

    * Uses 'fromIntegral' for the lossless conversion and 'toIntegralSized' for the other direction.
-}
{-# INLINE int8ToInt16 #-}
int8ToInt16 :: Prism' Int16 Int8
int8ToInt16 = prism' fromIntegral toIntegralSized

{- | Prism for conversion between 'Int8' and 'Int32'.

note(s):

    * Uses 'fromIntegral' for the lossless conversion and 'toIntegralSized' for the other direction.
-}
{-# INLINE int8ToInt32 #-}
int8ToInt32 :: Prism' Int32 Int8
int8ToInt32 = prism' fromIntegral toIntegralSized

{- | Prism for conversion between 'Int8' and 'Int64'.

note(s):

    * Uses 'fromIntegral' for the lossless conversion and 'toIntegralSized' for the other direction.
-}
{-# INLINE int8ToInt64 #-}
int8ToInt64 :: Prism' Int64 Int8
int8ToInt64 = prism' fromIntegral toIntegralSized

{- | Prism for conversion between 'Int8' and 'Int'.

note(s):

    * Uses 'fromIntegral' for the lossless conversion and 'toIntegralSized' for the other direction.
-}
{-# INLINE int8ToInt #-}
int8ToInt :: Prism' Int Int8
int8ToInt = prism' fromIntegral toIntegralSized

{- | Prism for conversion between 'Int16' and 'Int32'.

note(s):

    * Uses 'fromIntegral' for the lossless conversion and 'toIntegralSized' for the other direction.
-}
{-# INLINE int16ToInt32 #-}
int16ToInt32 :: Prism' Int32 Int16
int16ToInt32 = prism' fromIntegral toIntegralSized

{- | Prism for conversion between 'Int16' and 'Int64'.

note(s):

    * Uses 'fromIntegral' for the lossless conversion and 'toIntegralSized' for the other direction.
-}
{-# INLINE int16ToInt64 #-}
int16ToInt64 :: Prism' Int64 Int16
int16ToInt64 = prism' fromIntegral toIntegralSized

{- | Prism for conversion between 'Int16' and 'Int'.

note(s):

    * Uses 'fromIntegral' for the lossless conversion and 'toIntegralSized' for the other direction.
-}
{-# INLINE int16ToInt #-}
int16ToInt :: Prism' Int Int16
int16ToInt = prism' fromIntegral toIntegralSized

{- | Prism for conversion between 'Int32' and 'Int64'.

note(s):

    * Uses 'fromIntegral' for the lossless conversion and 'toIntegralSized' for the other direction.
-}
{-# INLINE int32ToInt64 #-}
int32ToInt64 :: Prism' Int64 Int32
int32ToInt64 = prism' fromIntegral toIntegralSized

{- | Prism for conversion between 'Int32' and 'Int'.

note(s):

    * This prism is only valid if 'Int' has 32 bits or more. This is the common case, but is not
    guaranteed by the Haskell report.

    * Uses 'fromIntegral' for the lossless conversion and 'toIntegralSized' for the other direction.
-}
{-# INLINE int32ToInt #-}
int32ToInt :: Prism' Int Int32
int32ToInt = prism' fromIntegral toIntegralSized
