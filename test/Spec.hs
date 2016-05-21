module Main where

import Test.Framework

import qualified Test.Data.SymReg.Parser as Parser

main :: IO ()
main = defaultMainWithOpts [
    Parser.testModule
  ]
  mempty
