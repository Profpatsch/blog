{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, LambdaCase #-}
module Profpatsch.Blog.Converter
( readPost
, extractPostMetadata
) where

import Protolude
import Data.Default (def)
import Data.Either.Validation (Validation(..), eitherToValidation)
import qualified Text.Pandoc as P
import Text.Pandoc (Pandoc)
import Text.Pandoc.Readers.Markdown as PRM
import Text.Pandoc.Writers.HTML as PWH
import qualified Text.Pandoc.FromMeta as PFM
import qualified Development.Shake as S

import Profpatsch.Blog.Types

readPost :: FilePath -- ^ post path
         -> S.Action Pandoc
readPost fp = do
  raw <- PRM.readMarkdownWithWarnings def <$> S.readFile' fp
  case raw of
    Left err -> liftIO $ throwIO err
    Right (pdc, warnings) -> do
      for_ warnings S.putNormal
      pure pdc

extractPostMetadata :: Pandoc
                    -> Validation (FilePath -> [Text]) PostMeta
                    -- ^ Concats meta field error messages
extractPostMetadata (P.Pandoc meta _) =
  let mdLine = "markdown line"
  in PostMeta
  <$> fm "title" mdLine
  <*> (optional $ fm "subtitle" mdLine)
  <*> (optional $ fm "tags" "list of string")
  where
    fm :: PFM.FromMetaValue a => [Char] -> [Char]
       -> Validation (FilePath -> [Text]) a
    fm key type_ =
      eitherToValidation $ first mkError $ PFM.fromMeta
        (PWH.writeHtmlString def . PFM.asPandoc) key meta
      where
        mkError :: PFM.MetaFieldError -> FilePath -> [Text]
        mkError PFM.MetaFieldMissing fn = [toS $
          fn <> ": meta data field for \"" <> key <> "\" missing"]
        mkError PFM.WrongType fn = [toS $
          fn <> ": " <> key <> " needs to be a " <> type_ <> " but is a "
          <> metaType (maybe (panic "key should exist!") identity
                        (P.lookupMeta key meta))]
    metaType :: P.MetaValue -> [Char]
    metaType = \case
      P.MetaMap _     -> "map"
      P.MetaList _    -> "list"
      P.MetaBool _    -> "boolean"
      P.MetaString _  -> "string"
      P.MetaInlines _ -> "string"
      P.MetaBlocks _  -> "paragraph"
