{-# LANGUAGE GeneralizedNewtypeDeriving, LambdaCase, FlexibleInstances #-}
module Text.Pandoc.FromMeta
( MetaPandoc, MetaPandocConv
, MetaFieldError(..)
, fromMeta, asPandoc
, FromMetaValue(..)
)where

import Data.Traversable (for)
import qualified Data.Map as M
import qualified Data.Text as T
import Text.Pandoc.Definition


-- | Possible "Text.Pandoc.Definition" value inside a 'MetaValue'.
type MetaPandoc = Either [Inline] [Block]
type MetaPandocConv = MetaPandoc -> String

data MetaFieldError
  = MetaFieldMissing
  | WrongType
  deriving (Show)

fromMeta :: (FromMetaValue a)
         => MetaPandocConv
         -> String
         -- ^ key of meta entry
         -> Meta
         -> Either MetaFieldError a
fromMeta conv key m = note MetaFieldMissing (lookupMeta key m)
           >>= note WrongType . fromMetaValue conv
  where note a = maybe (Left a) Right

-- | Helper function to be able to use writers with 'fromMetaValue'.
asPandoc :: MetaPandoc -> Pandoc
asPandoc = either (Pandoc mempty . pure . Plain) (Pandoc mempty)

-- | Parse a 'MetaValue' into a data type
class FromMetaValue a where
  fromMetaValue :: MetaPandocConv
                -> MetaValue
                -> Maybe a

fromMetaString :: MetaPandocConv -> MetaValue -> Maybe [Char]
fromMetaString conv = \case
    MetaString  s  -> Just s
    MetaInlines is -> Just . conv $ Left is
    MetaBlocks  bs -> Just . conv $ Right bs
    _              -> Nothing

instance FromMetaValue [Char] where
  fromMetaValue = fromMetaString

instance FromMetaValue T.Text where
  fromMetaValue conv = fmap T.pack . fromMetaString conv

instance FromMetaValue Bool where
  fromMetaValue _ = \case
    MetaBool b -> Just b
    _          -> Nothing

instance FromMetaValue a => FromMetaValue (M.Map [Char] a) where
  fromMetaValue conv = \case
    MetaMap mvs -> for mvs $ fromMetaValue conv
    _           -> Nothing

instance FromMetaValue a => FromMetaValue [a] where
  fromMetaValue conv = \case
    MetaList mvs -> for mvs $ fromMetaValue conv
    _            -> Nothing
