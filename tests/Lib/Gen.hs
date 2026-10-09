{- Some generators used in testing. -}

module Lib.Gen (
    -- * Generators.
    word8,
    tuple8Bool,
) where

-- Imports.
-- Base.
import Data.Word (Word8, Word64)

-- Testing.
import Data.Falsify.WordN (Precision (..), forgetPrecision)
import Test.Falsify (Gen)
import Test.Falsify.Generator (wordN, bool)


{- | 'Word8' generator. Biased towards zero. -}
word8 :: Gen Word8
word8 = fmap (fromIntegral @Word64 @Word8 . forgetPrecision) $ wordN (Precision 8)

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
