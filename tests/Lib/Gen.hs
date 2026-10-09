{- Some generators used in testing. -}

module Lib.Gen (
    -- * Primitive generators.
    word8,
    word16,
    word32,

    -- * Tuple generators.
    tupleWord8,
    tuple4Word8,
    tuple8Word8,
    tuple8Bool,

    -- * Enumeration generators.
    enumFull,
) where

-- Imports.
-- Base.
import Data.Word (Word8, Word16, Word32, Word64)

-- Testing.
import Data.Falsify.WordN (Precision (..), forgetPrecision)
import Test.Falsify (Gen)
import Test.Falsify.Generator (wordN, bool, inRange)
import Test.Falsify.Range (enum)


{- | 'Word8' generator. Biased towards zero. -}
word8 :: Gen Word8
word8 = fmap (fromIntegral @Word64 @Word8 . forgetPrecision) $ wordN (Precision 8)

{- | 'Word16' generator. Biased towards zero. -}
word16 :: Gen Word16
word16 = fmap (fromIntegral @Word64 @Word16 . forgetPrecision) $ wordN (Precision 16)

{- | 'Word32' generator. Biased towards zero. -}
word32 :: Gen Word32
word32 = fmap (fromIntegral @Word64 @Word32 . forgetPrecision) $ wordN (Precision 32)


{- | Generator for tuples of 'Word8'. -}
tupleWord8 :: Gen (Word8, Word8)
tupleWord8 = (,) <$> word8 <*> word8

{- | Generator for 4-tuples of 'Word8'. -}
tuple4Word8 :: Gen (Word8, Word8, Word8, Word8)
tuple4Word8 = (,,,) <$> word8 <*> word8 <*> word8 <*> word8

{- | Generator for 8-tuples of 'Word8'. -}
tuple8Word8 :: Gen (Word8, Word8, Word8, Word8, Word8, Word8, Word8, Word8)
tuple8Word8 = (,,,,,,,)
    <$> word8
    <*> word8
    <*> word8
    <*> word8
    <*> word8
    <*> word8
    <*> word8
    <*> word8

{- | Generator for 8-tuples of 'Bool'. -}
tuple8Bool :: Gen (Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool)
tuple8Bool = (,,,,,,,)
    <$> bool False
    <*> bool False
    <*> bool False
    <*> bool False
    <*> bool False
    <*> bool False
    <*> bool False
    <*> bool False

{- | Generator for the full range of a bounded enumeration. -}
enumFull :: (Enum a, Bounded a) => Gen a
enumFull = inRange $ enum (minBound, maxBound)
