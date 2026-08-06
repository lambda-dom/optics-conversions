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
    word64BytesLe,
    word64BytesBe,
) where


-- Imports.
-- Base.
import Data.Bits (Bits (..), FiniteBits (..))
import Data.Word (Word8, Word16, Word32, Word64)

-- Libraries.
import Optics.Core (Lens', Iso', lens, iso, view, set, review)

-- Package.
import Data.Word.Optics (word8ToWord16, word8ToWord32, word8ToWord64)


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
{-# INLINE bitAt #-}
bitAt :: forall a . Bits a => Int -> Lens' a Bool
bitAt i = lens project update
    where
        project :: a -> Bool
        project n = testBit n i

        update :: a -> Bool -> a
        update n b = if b then setBit n i else clearBit n i


{- | Generic byte lens.

note(s):

    * The lens is only valid if the integral type has 8 bits or more.

    * The result is undefined if the argument is negative or larger than the number of bytes in the
    type.
-}
{-# INLINE byteAt #-}
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
{-# INLINE bitsLe #-}
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
{-# INLINE bitsBe #-}
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
{-# INLINE word16BytesLe #-}
word16BytesLe :: Iso' Word16 (Word8, Word8)
word16BytesLe = iso from to
    where
        from :: Word16 -> (Word8, Word8)
        from n = (view (byteAt 0) n, view (byteAt 1) n)

        to :: (Word8, Word8) -> Word16
        to (m, n) = let h = review word8ToWord16 in h m .|. shiftL (h n) 8

{- | Isomorphism between 'Word16' and tuples of 'Word8'.

note(s):

    * The order of the bytes is big-endian, that is, most to least significant.

=== __Examples:__

>>> view word16BytesBe 0xff00
(255,0)
-}
{-# INLINE word16BytesBe #-}
word16BytesBe :: Iso' Word16 (Word8, Word8)
word16BytesBe = iso from to
    where
        from :: Word16 -> (Word8, Word8)
        from n = (view (byteAt 1) n, view (byteAt 0) n)

        to :: (Word8, Word8) -> Word16
        to (m, n) = let h = review word8ToWord16 in h n .|. shiftL (h m) 8

{- | Isomorphism between 'Word32' and 4-tuples of 'Word8'.

note(s):

    * The order of the bytes is little-endian, that is, least to most significant.

=== __Examples:__

>>> view word32BytesLe 0xff00
(0,255,0,0)
-}
{-# INLINE word32BytesLe #-}
word32BytesLe :: Iso' Word32 (Word8, Word8, Word8, Word8)
word32BytesLe = iso from to
    where
        from :: Word32 -> (Word8, Word8, Word8, Word8)
        from n = (view (byteAt 0) n, view (byteAt 1) n, view (byteAt 2) n, view (byteAt 3) n)

        to :: (Word8, Word8, Word8, Word8) -> Word32
        to (m, n, o, p) = let h = review word8ToWord32 in
                h m
            .|. shiftL (h n) 8
            .|. shiftL (h o) 16
            .|. shiftL (h p) 24

{- | Isomorphism between 'Word32' and 4-tuples of 'Word8'.

note(s):

    * The order of the bytes is big-endian, that is, most to least significant.

=== __Examples:__

>>> view word32BytesBe 0xff00
(0,0,255,0)
-}
{-# INLINE word32BytesBe #-}
word32BytesBe :: Iso' Word32 (Word8, Word8, Word8, Word8)
word32BytesBe = iso from to
    where
        from :: Word32 -> (Word8, Word8, Word8, Word8)
        from n = (view (byteAt 3) n, view (byteAt 2) n, view (byteAt 1) n, view (byteAt 0) n)

        to :: (Word8, Word8, Word8, Word8) -> Word32
        to (m, n, o, p) =  let h = review word8ToWord32 in
                h p
            .|. shiftL (h o) 8
            .|. shiftL (h n) 16
            .|. shiftL (h m) 24

{- | Isomorphism between 'Word64' and 8-tuples of 'Word8'.

note(s):

    * The order of the bytes is little-endian, that is, least to most significant.

=== __Examples:__

>>> view word64BytesLe 0xff00
(0,255,0,0,0,0,0,0)
-}
{-# INLINE word64BytesLe #-}
word64BytesLe :: Iso' Word64 (Word8, Word8, Word8, Word8, Word8, Word8, Word8, Word8)
word64BytesLe = iso from to
    where
        from :: Word64 -> (Word8, Word8, Word8, Word8, Word8, Word8, Word8, Word8)
        from n = (
                    view (byteAt 0) n,
                    view (byteAt 1) n,
                    view (byteAt 2) n,
                    view (byteAt 3) n,
                    view (byteAt 4) n,
                    view (byteAt 5) n,
                    view (byteAt 6) n,
                    view (byteAt 7) n
                )

        to :: (Word8, Word8, Word8, Word8, Word8, Word8, Word8, Word8) -> Word64
        to (m, n, o, p, q, r, s, t) = let h = review word8ToWord64 in
                h m
            .|. shiftL (h n) 8
            .|. shiftL (h o) 16
            .|. shiftL (h p) 24
            .|. shiftL (h q) 32
            .|. shiftL (h r) 40
            .|. shiftL (h s) 48
            .|. shiftL (h t) 56

{- | Isomorphism between 'Word64' and 8-tuples of 'Word8'.

note(s):

    * The order of the bytes is big-endian, that is, most to least significant.

=== __Examples:__

>>> view word64BytesBe 0xff00
(0,0,0,0,0,0,255,0)
-}
{-# INLINE word64BytesBe #-}
word64BytesBe :: Iso' Word64 (Word8, Word8, Word8, Word8, Word8, Word8, Word8, Word8)
word64BytesBe = iso from to
    where
        from :: Word64 -> (Word8, Word8, Word8, Word8, Word8, Word8, Word8, Word8)
        from n = (
                    view (byteAt 7) n,
                    view (byteAt 5) n,
                    view (byteAt 5) n,
                    view (byteAt 4) n,
                    view (byteAt 3) n,
                    view (byteAt 2) n,
                    view (byteAt 1) n,
                    view (byteAt 0) n
                )

        to :: (Word8, Word8, Word8, Word8, Word8, Word8, Word8, Word8) -> Word64
        to (m, n, o, p, q, r, s, t) =  let h = review word8ToWord64 in
                h t
            .|. shiftL (h s) 8
            .|. shiftL (h r) 16
            .|. shiftL (h q) 24
            .|. shiftL (h p) 32
            .|. shiftL (h o) 40
            .|. shiftL (h n) 48
            .|. shiftL (h m) 56
