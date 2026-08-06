{- |
Module: Data.Sequence.Optics

Optics for conversions from and to t'Seq'.
-}

module Data.Sequence.Optics (
    -- * Isomorphisms.
    seqToList,
) where

-- Imports.
-- Base.
import Data.Foldable (toList)

-- Libraries.
import Optics.Core (Iso', iso)
import Data.Sequence (Seq, fromList)


{- | Isomorphism between @'Seq' a@ and @[a]@.

note(s):

    * The isomorphism is only valid for _finite_ lists.

    * Uses 'toList' and 'fromList'.
-}
{-# INLINE seqToList #-}
seqToList :: Iso' (Seq a) [a]
seqToList = iso toList fromList
