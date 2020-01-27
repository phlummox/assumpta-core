
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Main where

import Data.Monoid
import Test.DocTest
import System.FilePath.Glob
import System.IO
import Shelly as Sh
import qualified Data.Text as T
import System.Environment

-- Requires the test-doctests flag to be enabled,
-- and the STACK_RESOLVER environment variable to
-- be defined (specifying a resolver to use, e.g.
-- "lts-14").

default (T.Text)

{-# ANN module ("HLint: ignore Eta reduce" :: String) #-}
{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}


withText :: (String -> String) -> T.Text -> T.Text
withText f = T.pack . f . T.unpack

get_packages :: Sh [T.Text]
get_packages =
  do
    let examplesDir = "examples"
    T.words <$> run "ghc-pkg" ["list", "--simple-output", "--names-only"]

bad_packages :: [T.Text]
bad_packages = [
    "cryptohash"
  , "cryptohash-sha256"
  ]

-- | if they are present, try to hide packages
-- which will clash
get_package_hiding_opts :: Sh [String]
get_package_hiding_opts = do
  packages <- get_packages
  return $ concat
        [ args | pkg <- packages,
                 pkg `elem` bad_packages,
                 let args = map T.unpack ["-hide-package", pkg]]


must_have_stack_resolver :: Sh ()
must_have_stack_resolver =
  get_env "STACK_RESOLVER" >>= \case
      Nothing -> terror "STACK_RESOLVER env var not set, exiting"
      Just{}  -> return ()

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  args <- getArgs
  putStrLn $ "\ndoctest-test. args: " ++ show args
  package_hiding_opts <- shelly $ silently $ do
    must_have_stack_resolver
    -- hide some packages which are quite likely to be installed,
    -- and which cause doctest to melt down with a
    -- "Ambiguous module name ‘XXX’..." error.
    get_package_hiding_opts
  srcFiles <- globDir1 (compile "**/*.hs") "src"
  putStrLn $ "source files being tested: " ++ show srcFiles
  let doctestOpts :: [String]
      doctestOpts = ["-isrc"] <> args
                     <> package_hiding_opts
  putStrLn $ "docTestOpts: " ++ show doctestOpts
  hFlush stdout
  -- actually run tests
  doctest $ doctestOpts ++ srcFiles

