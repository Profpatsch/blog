module Main where

import Data.Text.Encoding as TEnc
import Data.ByteString as BS

import CMarkGFM as M

main = do
  t <- TEnc.decodeUtf8 <$> BS.getContents
  let out = TEnc.encodeUtf8 $ M.commonmarkToHtml [] [] t
  BS.putStr out
