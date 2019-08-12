{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}

module Main (main) where

import Prelude ()
import Prelude.Compat

import Test.Tasty

import qualified TestCst
import qualified TestCompiler
import qualified TestCoreFn
import qualified TestDocs
import qualified TestHierarchy
import qualified TestPrimDocs
import qualified TestPsci
import qualified TestIde
import qualified TestPscPublish
import qualified TestBundle
import qualified TestUtils

import System.IO (hSetEncoding, stdout, stderr, utf8)

-- Disabling tests for purerl as calls out to node have not yet been replaced, for example
disableErlTests :: Bool
disableErlTests = True

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8

  heading "Updating support code"
  TestUtils.updateSupportCode

  cstTests <- TestCst.main
  ideTests <- TestIde.main
  heading "compiler test suite"
  compilerTests <- TestCompiler.main

  heading "PSCI test suite"
  psciTests <- TestPsci.main
  pscBundleTests <- TestBundle.main
  coreFnTests <- TestCoreFn.main
  docsTests <- TestDocs.main
  primDocsTests <- TestPrimDocs.main
  publishTests <- TestPscPublish.main
  hierarchyTests <- TestHierarchy.main

  defaultMain $
    testGroup
      "Tests"
      [ cstTests
      , compilerTests
      , psciTests
      , pscBundleTests
      , ideTests
      , coreFnTests
      , docsTests
      , primDocsTests
      , publishTests
      , hierarchyTests
      ]

  where
  heading msg = do
    putStrLn ""
    putStrLn $ replicate 79 '#'
    putStrLn $ "# " ++ msg
    putStrLn $ replicate 79 '#'
    putStrLn ""
