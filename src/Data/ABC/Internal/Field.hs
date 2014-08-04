{- |
Module      : Data.ABC.GIA
Copyright   : Galois, Inc. 2010-2014
License     : BSD3
Maintainer  : jhendrix@galois.com
Stability   : experimental
Portability : portable
-}
module Data.ABC.Internal.Field
  ( Field(..)
  , fieldFromOffset
  , isoFieldTarget
  , Iso
  , iso
  ) where

import Foreign

data Field a b = Field { readAt :: a -> IO b
                       , writeAt :: a -> b -> IO ()
                       }

fieldFromOffset :: Storable b => Int -> Field (Ptr a) b
fieldFromOffset o = Field (`peekByteOff` o) (`pokeByteOff` o)

isoFieldTarget :: Field a b -> Iso b c -> Field a c
isoFieldTarget (Field r w) (Iso f t) =
  Field { readAt = fmap f . r
        , writeAt = \b v -> w b (t v)
        }


-- | An isomorphism view.
data Iso a b = Iso (a -> b) (b -> a)

-- | Create an isomorphism
iso :: (a -> b) -> (b -> a) -> Iso a b
iso = Iso
