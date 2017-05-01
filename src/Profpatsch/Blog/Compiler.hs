{-# LANGUAGE OverloadedStrings, NoImplicitPrelude, LambdaCase #-}
module Profpatsch.Blog.Compiler
( compileBlog, compileBlogDefaultMain
) where

import Protolude

-- import qualified System.FilePath as FP
import Development.Shake
import qualified Development.Shake.FilePath as FP
import qualified Text.DocTemplates as Tmpl
import qualified Data.Aeson as A
import qualified Data.Either.Validation as V
import qualified Data.Text as T
import System.Exit (exitFailure)

import Profpatsch.Blog.Types
import qualified Profpatsch.Blog.Converter as BConv

templateFolder, postFolder, wipFolder :: FilePath
templateFolder = "templates"
postFolder = "posts"
wipFolder = postFolder FP.</> "wip"
postFileName = "post.md"

compileBlogDefaultMain :: IO ()
compileBlogDefaultMain = shakeArgs opts (compileBlog "_site")
  where opts = shakeOptions
                 { shakeLint = Just LintBasic
                 , shakeVerbosity = Chatty }

compileBlog :: FilePath -> Rules ()
compileBlog buildFolder = do
  want [index]

  index %> \out -> do
    postDirs <- filter (/= wipFolder)
              . map (postFolder FP.</>)
             <$> getDirectoryDirs postFolder
    metas <- for (foreach postDirs (FP.</> postFileName)) postToPostMeta
    tmpl <- applyTemplate (templateFolder FP.</> "index.html") metas
    writeFileT out tmpl

  where
    index = buildFolder FP.</> "index.html"

postToPostMeta :: FilePath -> Action PostMeta
postToPostMeta fp = do
  BConv.extractPostMetadata <$> BConv.readPost fp >>= \case
    V.Success pm -> pure pm
    V.Failure errf -> dieA $ T.intercalate "\n" $ errf fp

applyTemplate :: FilePath -- ^ template path
              -> [PostMeta] -- ^ metadata context
              -> Action Text
applyTemplate name ms = do
  let posts = A.object ["post" A..= A.toJSON ms ]
  liftIO $ print $ A.encode posts
  raw <- readFileT name
  case Tmpl.applyTemplate raw posts of
    Left err  -> dieA . toS
      $ "could not compile template " <> name <> "\n" <> err
    Right res -> pure res

readFileT :: FilePath -> Action Text
readFileT f = need [f] >> liftIO (readFile f)

writeFileT :: MonadIO m => FilePath -> Text -> m ()
writeFileT name f = liftIO $ writeFile name f

dieA :: Text -> Action a
dieA = liftIO . die . toS
