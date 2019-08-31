module Main where

import Control.Monad
import Text.Pandoc.Readers.Markdown
import Text.Pandoc
import Data.Default
import Data.Map as M
import Data.Monoid

raw :: IO String
raw = readFile "posts/nixos-on-lit-2017/post.md"

main = putPandoc

putPandoc = print $
  writeHtmlString def $ Pandoc mempty [Plain $ [Str "foo"]]

readAndPrintMeta = do
  s <- raw
  let Right (Pandoc m _) = readMarkdown def s
  forM_ (M.toList $ unMeta m) $ \i -> do
    putStr $ fst i
    putStr ": "
    putStrLn .show $  snd i
