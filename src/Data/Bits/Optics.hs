{- |
Module: Data.Bits.Optics

Optics for conversions from types with @'Bits'@ constraints.
-}

module Data.Bits.Optics (
    -- * Bit utilities.
    bitCount,
    byteCount,

    -- ** List functions.
    toIxBits,
    fromIxBits,
    toIxBytes,
    fromIxBytes,

    -- * Lenses for types with a 'Bits' constraint.
    bitAt,
    byteAt,

    -- * Indexed traversals for types with a 'FiniteBits' constraint.
    bits,
    bytes,

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
import Optics.Core (Lens', Iso', IxTraversal', lens, iso, view, set, review, itraversalVL)

-- Package.
import Data.Word.Optics (word8ToWord16, word8ToWord32, word8ToWord64)


-- $setup
-- >>> import Data.Word
-- >>> import Optics.Core


{- | Return the number of bits in @'FiniteBits' a => a@.

=== __Examples:__

>>> bitCount Word32
32
-}
{-# INLINE bitCount #-}
bitCount :: forall a -> FiniteBits a => Int
bitCount a = finiteBitSize @a zeroBits

{- | Return the number of bytes in @'FiniteBits' a => a@.

note(s):

    * Defined as @byteCount a = bitCount a `quot` 8@. Implicitly, the number of bits in @a@ must be
    a multiple of @8@.

=== __Examples:__

>>> byteCount Word32
4
-}
{-# INLINE byteCount #-}
byteCount :: forall a -> FiniteBits a => Int
byteCount a = bitCount a `quot` 8


{- | Return the enumerated list of bits in little-endian order.

=== __Examples:__

>>> toIxBits (15 :: Word8)
[(0,True),(1,True),(2,True),(3,True),(4,False),(5,False),(6,False),(7,False)]
-}
{-# INLINE toIxBits #-}
toIxBits :: forall a . FiniteBits a => a -> [(Int, Bool)]
toIxBits n = fmap (\ i -> (i, view (bitAt i) n)) [0 .. pred $ bitCount a]

{- | Pack a list of indexed bits as an @'Integral'@.

note(s):

    * Any elements in the list with index out of bounds are effectively discarded. Because of this
    'fromIxBits' is not a right-inverse to 'toIxBits'. It is neither a right inverse because
    trailing 'False' pairs are not required.

=== __Examples:__

>>> fromIxBits [(0, True),(1, True),(2, True),(3, True)] :: Word8
15
-}
{-# INLINE fromIxBits #-}
fromIxBits :: forall a . (Integral a, FiniteBits a) => [(Int, Bool)] -> a
fromIxBits = foldl' (.|.) 0 . fmap f
    where
        f :: (Int, Bool) -> a
        f (n, b) = if b then bit n else 0


{- | Return the enumerated list of bytes in little-endian order.

=== __Examples:__

>>> toIxBytes (65535 :: Word32)
[(0,255),(1,255),(2,0),(3,0)]
-}
{-# INLINE toIxBytes #-}
toIxBytes :: forall a . (Integral a, FiniteBits a) => a -> [(Int, Word8)]
toIxBytes n = fmap (\ i -> (i, view (byteAt i) n)) [0 .. pred $ byteCount a]

{- | Pack a list of indexed bytes as an @'Integral'@.

=== __Examples:__

>>> fromIxBytes [(0, 0xff),(1, 0xff)] :: Word32
65535
-}
{-# INLINE fromIxBytes #-}
fromIxBytes :: forall a . (Integral a, FiniteBits a) => [(Int, Word8)] -> a
fromIxBytes = foldl' (.|.) 0 . fmap f
    where
        f :: (Int, Word8) -> a
        f (i, m) = shiftL (fromIntegral @Word8 @a m) (8 * i)


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
        project n = fromIntegral @a @Word8 $ shiftR (shiftL 0xff (8 * i) .&. n) (8 * i)

        update :: a -> Word8 -> a
        update n w = let m = shiftL (fromIntegral @Word8 @a w) (8 * i) in (n .&. complement m) .|. m


{- | Indexed bit traversal for integral types with a 'FiniteBits' constraint. -}
{-# INLINE bits #-}
bits :: forall a . (Integral a, FiniteBits a) => IxTraversal' Int a Bool
bits = itraversalVL h
    where
        h :: forall f . Applicative f => (Int -> Bool -> f Bool) -> a -> f a
        h f = fmap fromIxBits . sequenceA . fmap (\ (i, n) -> fmap (i, ) $ f i n) . toIxBits

{- | Indexed byte traversal for integral types with a 'FiniteBits' constraint. -}
{-# INLINE bytes #-}
bytes :: forall a . (Integral a, FiniteBits a) => IxTraversal' Int a Word8
bytes = itraversalVL h
    where
        h :: forall f . Applicative f => (Int -> Word8 -> f Word8) -> a -> f a
        h f = fmap fromIxBytes . sequenceA . fmap (\ (i, n) -> fmap (i, ) $ f i n) . toIxBytes


{- | Isomorphism between t'Word8' and 8-tuples of 'Bool' in little-endian order.

=== __Examples:__

>>> view bitsLe 0x04
(False,False,True,False,False,False,False,False)

>>> review bitsLe (False,False,True,False,False,False,False,False)
4
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

{- | Isomorphism between t'Word8' and 8-tuples of 'Bool' in big-endian order.

=== __Examples:__

>>> view bitsBe 0x04
(False,False,False,False,False,True,False,False)

>>> review bitsBe (False,False,False,False,False,True,False,False)
4
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

{- | Isomorphism between 'Word16' and tuples of 'Word8' in little-endian order.

=== __Examples:__

>>> view word16BytesLe 0xff00
(0,255)

>>> review word16BytesLe (0,255)
65280
-}
{-# INLINE word16BytesLe #-}
word16BytesLe :: Iso' Word16 (Word8, Word8)
word16BytesLe = iso from to
    where
        from :: Word16 -> (Word8, Word8)
        from n = (view (byteAt 0) n, view (byteAt 1) n)

        to :: (Word8, Word8) -> Word16
        to (m, n) = let h = review word8ToWord16 in h m .|. shiftL (h n) 8

{- | Isomorphism between 'Word16' and tuples of 'Word8' in big-endian order.

=== __Examples:__

>>> view word16BytesBe 0xff00
(255,0)

>>> review word16BytesBe (255, 0)
65280
-}
{-# INLINE word16BytesBe #-}
word16BytesBe :: Iso' Word16 (Word8, Word8)
word16BytesBe = iso from to
    where
        from :: Word16 -> (Word8, Word8)
        from n = (view (byteAt 1) n, view (byteAt 0) n)

        to :: (Word8, Word8) -> Word16
        to (m, n) = let h = review word8ToWord16 in h n .|. shiftL (h m) 8

{- | Isomorphism between 'Word32' and 4-tuples of 'Word8' in little-endian order.

=== __Examples:__

>>> view word32BytesLe 0xff00
(0,255,0,0)

>>> review word32BytesLe (0,255,0,0)
65280
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

{- | Isomorphism between 'Word32' and 4-tuples of 'Word8' in big-endian order.

=== __Examples:__

>>> view word32BytesBe 0xff00
(0,0,255,0)

>>> review word32BytesBe (0,0,255,0)
65280
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

{- | Isomorphism between 'Word64' and 8-tuples of 'Word8' in little-endian order.

=== __Examples:__

>>> view word64BytesLe 0xff00
(0,255,0,0,0,0,0,0)

>>> review word64BytesLe (0,255,0,0,0,0,0,0)
65280
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

{- | Isomorphism between 'Word64' and 8-tuples of 'Word8' in big-endian order.

=== __Examples:__

>>> view word64BytesBe 0xff00
(0,0,0,0,0,0,255,0)

>>> review word64BytesBe (0,0,0,0,0,0,255,0)
65280
-}
{-# INLINE word64BytesBe #-}
word64BytesBe :: Iso' Word64 (Word8, Word8, Word8, Word8, Word8, Word8, Word8, Word8)
word64BytesBe = iso from to
    where
        from :: Word64 -> (Word8, Word8, Word8, Word8, Word8, Word8, Word8, Word8)
        from n = (
                    view (byteAt 7) n,
                    view (byteAt 6) n,
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
