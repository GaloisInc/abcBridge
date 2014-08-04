{- |
Module      : Data.ABC.Util
Copyright   : Galois, Inc. 2010-2014
License     : BSD3
Maintainer  : jhendrix@galois.com
Stability   : experimental
Portability : portable
-}
module Data.ABC.Util
  ( forN_
  , forN
  ) where


forN_ :: Monad m => Int -> (Int -> m ()) -> m ()
forN_ i m | i <= 0 = return ()
          | otherwise = m (i-1) >> forN_ (i-1) m

forN :: Monad m => Int -> (Int -> m a) -> m [a]
forN i0 m = go [] i0
  where go l i | i <= 0 = return (reverse l)
               | otherwise = m (i-1) >>= \e -> go (e:l) (i-1)
