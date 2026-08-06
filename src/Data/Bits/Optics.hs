{- |
Module: Data.Bits.Optics

Optics for conversions from types with @'Bits'@ constraints.
-}

module Data.Bits.Optics (
    -- * Utility functions.
    bitCount,
    byteCount,

    -- * Lenses for types with a 'Bits' constraint.
    bitAt,
    byteAt,

    -- * Isomorphisms for types with a 'Bits' constraint.
    bitsLe,
    bitsBe,
    word16BytesLe,
    word16BytesBe,
    word32BytesLe,
    word32BytesBe,
) where


-- Imports.
-- Base.
import Data.Bits (Bits (..), FiniteBits (..))
import Data.Word (Word8, Word16, Word32)

-- Libraries.
import Optics.Core (Lens', Iso', lens, iso, view, set)


-- $setup
-- >>> import Data.Word
-- >>> import Optics.Core


{- | Return the number of bits in the integral type.

=== __Examples:__

>>> bitCount Word8
8

>>> bitCount Int
64
-}
{-# INLINE bitCount #-}
bitCount :: forall a -> FiniteBits a => Int
bitCount a = finiteBitSize (zeroBits @a)

{- | Return the number of bytes in the integral type.

note(s):

  * It is implicitely assumed that the number of bits is a positive multiple of @8@.

=== __Examples:__

>>> byteCount Word8
1

>>> byteCount Int
8
-}
{-# INLINE byteCount #-}
byteCount :: forall a -> FiniteBits a => Int
byteCount a = bitCount a `quot` bitCount Word8


{- | Generic bit flag lens.

note(s):

    * The result is undefined if the argument is negative or larger than the number of bits in the
    type.
-}
{-# INLINEABLE bitAt #-}
bitAt :: forall a . Bits a => Int -> Lens' a Bool
bitAt i = lens project update
    where
        project :: a -> Bool
        project n = testBit n i

        update :: a -> Bool -> a
        update n b = if b then setBit n i else clearBit n i


{- | Generic byte lens.

note(s):

    * The result is undefined if the argument is negative or larger than the number of bytes in the
    type.
-}
{-# INLINEABLE byteAt #-}
byteAt :: forall a . (Integral a, Bits a) => Int -> Lens' a Word8
byteAt i = lens project update
    where
        project :: a -> Word8
        project n = fromIntegral $ shiftR (shiftL 0xff (8 * i) .&. n) (8 * i)

        update :: a -> Word8 -> a
        update n w = let m = shiftL (fromIntegral w) (8 * i) in (n .&. complement m) .|. m


{- | Isomorphism between t'Word8' and 8-tuples of 'Bool'.

note(s):

    * The order of the bits is little-endian, that is, least to most significant.

=== __Examples:__

>>> view bitsLe 0x04
(False,False,True,False,False,False,False,False)
-}
{-# INLINEABLE bitsLe #-}
bitsLe :: Iso' Word8 (Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool)
bitsLe = iso from to
    where
        from :: Word8 -> (Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool)
        from n = (
                    view (bitAt 0) n,
                    view (bitAt 1) n,
                    view (bitAt 2) n,
                    view (bitAt 3) n,
                    view (bitAt 4) n,
                    view (bitAt 5) n,
                    view (bitAt 6) n,
                    view (bitAt 7) n
                )

        to :: (Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool) -> Word8
        to (b0, b1, b2, b3, b4, b5, b6, b7)
            =   withBit 0 b0
            .|. withBit 1 b1
            .|. withBit 2 b2
            .|. withBit 3 b3
            .|. withBit 4 b4
            .|. withBit 5 b5
            .|. withBit 6 b6
            .|. withBit 7 b7
            where
                withBit :: Int -> Bool -> Word8
                withBit n b = set (bitAt n) b zeroBits 

{- | Isomorphism between t'Word8' and 8-tuples of 'Bool'.

note(s):

    * The order of the bits is big-endian, that is, most to least significant.

=== __Examples:__

>>> view bitsBe 0x04
(False,False,False,False,False,True,False,False)
-}
{-# INLINEABLE bitsBe #-}
bitsBe :: Iso' Word8 (Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool)
bitsBe = iso from to
    where
        from :: Word8 -> (Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool)
        from n = (
                    view (bitAt 7) n,
                    view (bitAt 6) n,
                    view (bitAt 5) n,
                    view (bitAt 4) n,
                    view (bitAt 3) n,
                    view (bitAt 2) n,
                    view (bitAt 1) n,
                    view (bitAt 0) n
                )

        to :: (Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool) -> Word8
        to (b0, b1, b2, b3, b4, b5, b6, b7)
            =   withBit 7 b0
            .|. withBit 6 b1
            .|. withBit 5 b2
            .|. withBit 4 b3
            .|. withBit 3 b4
            .|. withBit 2 b5
            .|. withBit 1 b6
            .|. withBit 0 b7
            where
                withBit :: Int -> Bool -> Word8
                withBit n b = set (bitAt n) b zeroBits

{- | Isomorphism between 'Word16' and tuples of 'Word8'.

note(s):

    * The order of the bytes is little-endian, that is, least to most significant.

=== __Examples:__

>>> view word16BytesLe 0xff00
(0,255)
-}
{-# INLINEABLE word16BytesLe #-}
word16BytesLe :: Iso' Word16 (Word8, Word8)
word16BytesLe = iso from to
    where
        from :: Word16 -> (Word8, Word8)
        from n = (view (byteAt 0) n, view (byteAt 1) n)

        to :: (Word8, Word8) -> Word16
        to (m, n) = fromIntegral m .|. shiftL (fromIntegral n) 8

{- | Isomorphism between 'Word16' and tuples of 'Word8'.

note(s):

    * The order of the bytes is big-endian, that is, most to least significant.

=== __Examples:__

>>> view word16BytesBe 0xff00
(255,0)
-}
{-# INLINEABLE word16BytesBe #-}
word16BytesBe :: Iso' Word16 (Word8, Word8)
word16BytesBe = iso from to
    where
        from :: Word16 -> (Word8, Word8)
        from n = (view (byteAt 1) n, view (byteAt 0) n)

        to :: (Word8, Word8) -> Word16
        to (m, n) = fromIntegral n .|. shiftL (fromIntegral m) 8

{- | Isomorphism between 'Word32' and 4-tuples of 'Word8'.

note(s):

    * The order of the bytes is little-endian, that is, least to most significant.

=== __Examples:__

>>> view word32BytesLe 0xff00
(0,255,0,0)
-}
{-# INLINEABLE word32BytesLe #-}
word32BytesLe :: Iso' Word32 (Word8, Word8, Word8, Word8)
word32BytesLe = iso from to
    where
        from :: Word32 -> (Word8, Word8, Word8, Word8)
        from n = (view (byteAt 0) n, view (byteAt 1) n, view (byteAt 2) n, view (byteAt 3) n)

        to :: (Word8, Word8, Word8, Word8) -> Word32
        to (m, n, o, p)
            =   fromIntegral m
            .|. shiftL (fromIntegral n) 8
            .|. shiftL (fromIntegral o) 16
            .|. shiftL (fromIntegral p) 24

{- | Isomorphism between 'Word32' and 4-tuples of 'Word8'.

note(s):

    * The order of the bytes is big-endian, that is, most to least significant.

=== __Examples:__

>>> view word32BytesBe 0xff00
(0,0,255,0)
-}
{-# INLINEABLE word32BytesBe #-}
word32BytesBe :: Iso' Word32 (Word8, Word8, Word8, Word8)
word32BytesBe = iso from to
    where
        from :: Word32 -> (Word8, Word8, Word8, Word8)
        from n = (view (byteAt 3) n, view (byteAt 2) n, view (byteAt 1) n, view (byteAt 0) n)

        to :: (Word8, Word8, Word8, Word8) -> Word32
        to (m, n, o, p)
            =   fromIntegral p
            .|. shiftL (fromIntegral o) 8
            .|. shiftL (fromIntegral n) 16
            .|. shiftL (fromIntegral m) 24
