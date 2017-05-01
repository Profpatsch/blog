module Main where

import Profpatsch.Blog.Compiler (compileBlogDefaultMain)
import Development.Shake

main :: IO ()
main = compileBlogDefaultMain
