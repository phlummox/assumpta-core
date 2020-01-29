
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Control.Monad
import Data.Char as Ch
import Data.Maybe
import Data.Monoid
import Shelly as Sh
import qualified Data.Text as T
import System.IO

-- Requires the test-doctests flag to be enabled,
-- and the STACK_RESOLVER environment variable to
-- be defined (specifying a resolver to use, e.g.
-- "lts-14").
--
default (T.Text)

{-# ANN module ("HLint: ignore Eta reduce" :: String) #-}
{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

isHaskSource :: Sh.FilePath -> Bool
isHaskSource p = "hs" `hasExt` p

basename :: Sh.FilePath -> Sh.FilePath -> Sh Sh.FilePath
basename dir file =
  relativeTo dir file

baseNameSatisfies ::
  (T.Text -> b) -> Sh.FilePath -> Sh.FilePath -> Sh b
baseNameSatisfies pred basedir file = pred . toTextIgnore  <$> basename basedir file

-- is stack on the path
we_must_have_stack :: Sh ()
we_must_have_stack =
  unlessM (isJust <$> which "stack") $ do
    echo "'stack' binary not found on path, exiting"
    terror "abort! abort!" 

-- are we running as part of 'stack test'?
-- If so, we can assume the library has been
-- built and is available to us.
we_must_be_run_by_stack :: Sh ()
we_must_be_run_by_stack =
  unlessM (isJust <$> get_env "STACK_EXE") $
    terror "STACK_EXE env var not set"

get_stack_resolver :: Sh T.Text
get_stack_resolver =
  get_env "STACK_RESOLVER" >>= \case
      Nothing  -> terror "STACK_RESOLVER env var not set, exiting"
      Just res -> return res

main :: IO ()
main =  
  do
    let examplesDir = "examples"
    hSetBuffering stdout LineBuffering
    shelly $ verbosely $ do
      echo "\n\n=================="
      echo     "Compiling Examples"
      echo     "==================\n"
      stack_resolver <- get_stack_resolver
      we_must_have_stack
      we_must_be_run_by_stack
      hsFiles <- (filter isHaskSource <$> ls examplesDir) >>=
                    filterM (baseNameSatisfies startsWithLetter examplesDir)

      forM_ hsFiles $ \hsFile -> do
        let hsFile' = toTextIgnore hsFile 
        echo $ "Compiling " <> hsFile' <> "\n"
        run "stack" ["ghc", "--resolver=" <> stack_resolver
                      , "--", "--make", toTextIgnore hsFile]

  where
    startsWithLetter = Ch.isLetter . T.head

