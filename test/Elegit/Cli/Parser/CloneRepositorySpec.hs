{-# LANGUAGE QuasiQuotes #-}

module Elegit.Cli.Parser.CloneRepositorySpec where

import Data.String.QQ
import Elegit.Cli.Command
import Elegit.Cli.Parser.Util as P
import Test.Hspec
import Universum

helpText :: Text
helpText =
  "Usage: git elegant clone-repository GIT_CLONE_ARG...\n"
    <> [s|

  Clones a remote repository and configures it.

Available options:
  GIT_CLONE_ARG...         For now we support all git-clone arguments just by
                           passing everything to git-clone command
  -h,--help                Show this help text

Clones a repository into a new directory and runs its configuration.

The command accepts everything that `git clone` command accepts.
Run `git clone --help` to see the available options.

Approximate commands flow is
```bash
==>> git elegant clone-repository --depth 5 git@github.com:bees-hive/elegant-git.git my-dir
git clone --depth 5 git@github.com:bees-hive/elegant-git.git my-dir
cd my-dir
git elegant acquire-repository
```|]

spec :: Spec
spec = do
  describe "cmd" $ do
    it "parses command succefully" $ do
      parseMaybe "clone-repository" ["path"]
        `shouldBe` Just (CloneRepositoryCommand $ CloneRepositoryData ("path" :| []))

    it "failes on no arguments" $ do
      parseResult "clone-repository" [] `shouldRender` helpText

    it "renders command help" $ do
      parseResult "clone-repository" ["--help"] `shouldRender` helpText

    it "failes to parse unexpected arguments" $ do
      parseMaybe "clone-repository" ["test-arg"]
        `shouldBe` Just (CloneRepositoryCommand $ CloneRepositoryData ("test-arg" :| []))
    it "failes to parse unexpected options" $ do
      parseMaybe "clone-repository" ["--test-arg"]
        `shouldBe` Just (CloneRepositoryCommand $ CloneRepositoryData ("--test-arg" :| []))

    it "parses arguments correctly" $ do
      parseMaybe "clone-repository" ["--depth", "5", "git@github.com:bees-hive/elegant-git.git", "my-dir"]
        `shouldBe` Just (CloneRepositoryCommand $ CloneRepositoryData ("--depth" :| ["5", "git@github.com:bees-hive/elegant-git.git", "my-dir"]))
