{- Some generators used in testing. -}

module Lib.Gen (
    -- * Generators for integral, primitive types.
    integral,
    
    -- ** Specializations for primitive types.
    word8,
    word16,
    word32,
    int8,
    int16,
    int32,

    -- * Tuple generators.
    pair,
    triple,
    quadruple,
    octuple,

    -- * Enumeration generators.
    enumFull,
) where

-- Imports.
-- Base.
import Data.Bits (FiniteBits)
import Data.Word (Word8, Word16, Word32)
import Data.Int (Int8, Int16, Int32)

-- Testing.
import Test.Falsify (Gen)
import Test.Falsify.Generator (inRange)
import Test.Falsify.Range (enum, uniform)


{- | Generator for bounded integral types with a finite number of bits. Biased towards zero. -}
integral :: forall a . (Integral a, Bounded a, FiniteBits a) => Gen a
integral = inRange $ uniform @a

{- | Specialization of 'integral' for 'Word8' generator. -}
word8 :: Gen Word8
word8 = integral

{- | Specialization of 'integral' for 'Word16' generator. -}
word16 :: Gen Word16
word16 = integral

{- | Specialization of 'integral' for 'Word32' generator. -}
word32 :: Gen Word32
word32 = integral

{- | Uniform generator for 'Int8'. -}
int8 :: Gen Int8
int8 = integral

{- | Specialization of 'integral' for 'Int16' generator. -}
int16 :: Gen Int16
int16 = integral

{- | Specialization of 'integral' for 'Int32' generator. -}
int32 :: Gen Int32
int32 = integral


{- | Generator for pairs. -}
pair :: Gen a -> Gen b -> Gen (a, b)
pair g h = (,) <$> g <*> h

{- | Generator for triples. -}
triple :: Gen a -> Gen b -> Gen c -> Gen (a, b, c)
triple f g h = (,,) <$> f <*> g <*> h

{- | Generator for quadruples, or 4-tuples. -}
quadruple :: Gen a -> Gen b -> Gen c -> Gen d -> Gen (a, b, c, d)
quadruple f g h i = (,,,) <$> f <*> g <*> h <*> i

{- | Generator for homogeneous 8-tuples. -}
octuple :: Gen a -> Gen (a, a, a, a, a, a, a, a)
octuple g = (,,,,,,,)
    <$> g
    <*> g
    <*> g
    <*> g
    <*> g
    <*> g
    <*> g
    <*> g

{- | Generator for the full range of a bounded enumeration. -}
enumFull :: (Enum a, Bounded a) => Gen a
enumFull = inRange $ enum (minBound, maxBound)
