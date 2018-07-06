{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.FastAnagrams where

import Course.Core
import Course.List
import Course.Functor
import qualified Data.Set as S

-- Return all anagrams of the given string
-- that appear in the given dictionary file.
fastAnagrams :: Chars -> FilePath -> IO (List Chars)
fastAnagrams w p = let d = ((NoCaseString <$>) . lines) <$> readFile p
                       a = NoCaseString <$> permutations w
                       r = intersectBy (==) a <$> d
                   in (ncString <$>) <$> r

newtype NoCaseString =
  NoCaseString {
    ncString :: Chars
  }

instance Eq NoCaseString where
  (==) = (==) `on` map toLower . ncString

instance Show NoCaseString where
  show = show . ncString
