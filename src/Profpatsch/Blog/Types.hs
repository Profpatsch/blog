{-# LANGUAGE NoImplicitPrelude, DeriveGeneric, GeneralizedNewtypeDeriving #-}
module Profpatsch.Blog.Types where

import Protolude

import qualified Data.Aeson as A
import GHC.Generics (Generic)

data PostMeta = PostMeta
  { title :: Text
  , subtitle :: Maybe Text
  , tags :: Maybe [Tag]
  } deriving (Show, Generic)

instance A.ToJSON PostMeta

type Tag = Text
