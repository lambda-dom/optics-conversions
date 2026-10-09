{- Some generators used in testing. -}

module Lib.Gen (
    -- * Primitive generators.
    word8,
    word16,
    word32,

    -- * Tuple generators.
    tupleWord8,
    tuple8Bool,
    tuple4Word8,
) where

-- Imports.
-- Base.
import Data.Word (Word8, Word16, Word32, Word64)

-- Testing.
import Data.Falsify.WordN (Precision (..), forgetPrecision)
import Test.Falsify (Gen)
import Test.Falsify.Generator (wordN, bool)


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


{- | Generator for 8-tuples of 'Bool'. -}
tuple8Bool :: Gen (Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool)
tuple8Bool = do
    x1 <- bool False
    x2 <- bool False
    x3 <- bool False
    x4 <- bool False
    x5 <- bool False
    x6 <- bool False
    x7 <- bool False
    x8 <- bool False
    pure (x1, x2, x3, x4, x5, x6, x7, x8)
