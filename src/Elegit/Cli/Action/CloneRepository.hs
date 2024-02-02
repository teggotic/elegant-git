{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Elegit.Cli.Action.CloneRepository
  ( cli
  , cmd
  ) where

import Control.Monad.Free.Class
import Data.String.QQ
import qualified Elegit.Cli.Action.AcquireRepository as AcquireRepository
import Elegit.Cli.Command
import qualified Elegit.Git.Action as GA
import Options.Applicative
import qualified Options.Applicative.Help.Pretty as OA
import System.FilePath (dropExtension)
import Text.URI (URI (uriPath), mkURI, unRText)
import Universum

purpose :: OA.Doc
purpose = OA.text "Clones a remote repository and configures it."

description :: OA.Doc
description =
  OA.string
    [s|
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

cli :: Mod CommandFields ElegitCommand
cli =
  command "clone-repository"
    $ info
      ( CloneRepositoryCommand
          <$> ( CloneRepositoryData
                  <$> someNE
                    ( strArgument
                        ( metavar "GIT_CLONE_ARG..."
                            <> help "For now we support all git-clone arguments just by passing everything to git-clone command"
                        )
                    )
              )
      )
    $ mconcat
      [ progDescDoc (Just purpose)
      , footerDoc (Just description)
      , forwardOptions
      ]

cmd :: (MonadFree GA.GitF m) => CloneRepositoryData -> m ()
cmd (CloneRepositoryData opts) = do
  let
    urlOrDirectory = last opts

  mDirectory <- case mkURI urlOrDirectory of
    Nothing -> pure $ Just urlOrDirectory
    Just uri ->
      case uriPath uri of
        Nothing -> pure Nothing
        Just (_, pathPieces) -> pure $ Just $ toText $ dropExtension $ toString $ unRText $ last pathPieces

  case mDirectory of
    Nothing -> do
      GA.print
        [s|
Failed to get PATH from provided URI.
Please provide an explicit path.|]
    Just directory -> do
      GA.cloneRepositoryVerbose opts >>= \case
        Nothing ->
          GA.print
            [s|
Seems like clone command has failed to run successfully.
Please invesitage the git output below for troubleshooting.
# TODO: Display error info here|]
        _ -> do
          GA.directoryExists directory >>= \case
            False ->
              GA.print
                [s|
We've failed to parse destination directory correctly. Aborting.
To proceed, cd into a cloned directory and run `acquire-repository` command.|]
            True -> do
              GA.withCWD directory $ do
                AcquireRepository.cmd
