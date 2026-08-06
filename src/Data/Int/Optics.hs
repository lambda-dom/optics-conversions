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
    intToWord,

    -- * Prisms for conversion between integral, signed primitive types.
) where

-- Imports.
-- Base.
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)

-- Libraries.
import Optics.Core (Iso', iso)


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

{- | Isomorphism for conversion between 'Int' and 'Word'.

note(s):

    * Uses 'fromIntegral' for both directions.
-}
{-# INLINE intToWord #-}
intToWord :: Iso' Int Word
intToWord = iso fromIntegral fromIntegral

