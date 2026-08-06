{- |
Module: Data.Vector.Optics

Optics for conversions from and to t'Vector'.
-}

module Data.Vector.Optics (
    -- * Isomorphisms.
    vectorToList,
) where

-- Imports.
-- Libraries.
import Data.Vector (Vector, toList, fromList)
import Optics.Core (Iso', iso)


{- | Isomorphism between @'Vector' a@ and @[a]@.

note(s):

    * The isomorphism is only valid for _finite_ lists.

    * Uses 'toList' and 'fromList'.
-}
{-# INLINE vectorToList #-}
vectorToList :: Iso' (Vector a) [a]
vectorToList = iso toList fromList
