{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Advent.Of.Code.Solution where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Lazy qualified as L
import GHC.Exts (IsList (..))

type Solution = Text -> IO String

class FromText a where
  fromText :: Text -> a

instance {-# OVERLAPPABLE #-} IsString a => FromText a where
  fromText = fromString . T.unpack
  {-# INLINE fromText #-}

instance {-# OVERLAPS #-} Read a => FromText [a] where
  fromText = fmap read . lines . T.unpack
  {-# INLINE fromText #-}

instance {-# OVERLAPS #-} Read a => FromText (NonEmpty a) where
  fromText = NE.fromList . fmap read . lines . T.unpack
  {-# INLINE fromText #-}

instance {-# OVERLAPPING #-} FromText Text where
  fromText = id
  {-# INLINE fromText #-}

instance {-# OVERLAPPING #-} FromText L.Text where
  fromText = L.fromStrict
  {-# INLINE fromText #-}

instance {-# OVERLAPPING #-} FromText [String] where
  fromText = lines . T.unpack
  {-# INLINE fromText #-}

instance {-# OVERLAPPING #-} FromText String where
  fromText = T.unpack
  {-# INLINE fromText #-}

instance {-# OVERLAPPING #-} FromText ByteString where
  fromText = T.encodeUtf8
  {-# INLINE fromText #-}

instance {-# OVERLAPPING #-} FromText LBS.ByteString where
  fromText = LBS.fromStrict . T.encodeUtf8
  {-# INLINE fromText #-}

class ToString a where
  toString :: a -> String

instance {-# OVERLAPPABLE #-} (IsList a, Item a ~ Char) => ToString a where
  toString = toList
  {-# INLINE toString #-}

instance {-# OVERLAPPING #-} ToString String where
  toString = id
  {-# INLINE toString #-}

instance {-# OVERLAPPING #-} ToString ByteString where
  toString = BS8.unpack
  {-# INLINE toString #-}

instance {-# OVERLAPPING #-} ToString LBS.ByteString where
  toString = LBS8.unpack
  {-# INLINE toString #-}

instance {-# OVERLAPPING #-} ToString Int where
  toString = show
  {-# INLINE toString #-}

instance {-# OVERLAPPING #-} ToString Word where
  toString = show
  {-# INLINE toString #-}

instance {-# OVERLAPPING #-} ToString Integer where
  toString = show
  {-# INLINE toString #-}

class IsSolution a where
  solve :: a -> Solution

instance {-# OVERLAPPABLE #-} (FromText a, ToString b) => IsSolution (a -> b) where
  solve f = pure . toString . f . fromText
  {-# INLINE solve #-}

instance {-# OVERLAPPABLE #-} (FromText a, ToString b) => IsSolution (a -> IO b) where
  solve f = fmap toString . f . fromText
  {-# INLINE solve #-}

instance {-# OVERLAPPING #-} FromText a => IsSolution (a -> IO String) where
  solve f = f . fromText
  {-# INLINE solve #-}
