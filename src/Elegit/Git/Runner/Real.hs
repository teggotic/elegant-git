{-# LANGUAGE TemplateHaskell #-}

module Elegit.Git.Runner.Real where

import Control.Monad.Free.Church
import Control.Monad.HT (until)
import Data.List.NonEmpty as NE (append, singleton, take)
import qualified Elegit.Git.Action as GA
import Elegit.Git.Exec (MonadGitExec (directoryExists, execGit, gLine, pText, pTextLn, setCWD, withFileWithText))
import Fmt
import Lens.Micro.Mtl
import Lens.Micro.TH (makeLenses)
import System.FilePath (joinPath)
import Universum as U hiding (use, withFile)

data ExecState = ExecState
  { _esCwd :: NonEmpty Text
  }

makeLenses ''ExecState

-- | Execute the action in the real world.
executeGit :: (MonadGitExec m, MonadState ExecState m) => GA.FreeGit () -> m ()
executeGit = foldF executeGitF

-- | Interpreter for the real world
executeGitF :: (MonadGitExec m, MonadState ExecState m) => GA.GitF a -> m a
executeGitF arg = case arg of
  GA.CloneRepository gc next -> do
    next <$> execGit gc
  GA.AppendToCWD path next -> do
    -- esCwd %= (`append` singleton path)
    use esCwd
      >>= setCWD . joinPath . fmap toString . toList
    return next
  GA.PopFromCWD next -> do
    -- esCwd %= (\cwd -> fromList $ NE.take (if length cwd > 1 then length cwd else 0) cwd)
    use esCwd
      >>= setCWD . joinPath . fmap toString . toList
    return next
  GA.DirectoryExists path next -> do
    next <$> directoryExists (toString path)
  GA.InitRepository gc next -> do
    void $ execGit gc
    return next
  GA.AddInitialCommit gc@(GA.GInitialCommitData commitMessage) next -> do
    withFileWithText "a-message-of-initial-commit" commitMessage $ do
      void $ execGit gc
    return next
  GA.CurrentBranch gc next -> do
    mCurrentBranch <- execGit gc
    return $ next mCurrentBranch
  GA.BranchUpstream gc next -> do
    mUpstreamBranch <- execGit gc
    return $ next mUpstreamBranch
  GA.Show gc next -> do
    output <- execGit gc
    return $ next $ fromMaybe [] output
  GA.Log gc next -> do
    logs <- fromMaybe [] <$> execGit gc
    return $ next logs
  GA.Status gc next -> do
    changes <- fromMaybe [] <$> execGit gc
    return $ next changes
  GA.StashList gc next -> do
    stashes <- fromMaybe [] <$> execGit gc
    return $ next stashes
  GA.PathToTool gc next -> do
    path <- execGit gc
    return $ next path
  GA.GPGListKeys gc next -> do
    next <$> execGit gc
  GA.AliasesToRemove gc next -> do
    next <$> execGit gc
  GA.ReadConfig gc next -> do
    next <$> execGit gc
  GA.SetConfig gc next -> do
    U.void $ execGit gc
    return next
  GA.UnsetConfig gc next -> do
    U.void $ execGit gc
    return next
  GA.Prompt (GA.PromptConfig prompt pType) next -> do
    let
      askPrompt = do
        pText (colored Purple Normal message)
        gLine

      message :: Text
      message =
        case pType of
          GA.PromptOneTime -> fmt "" +| prompt |+ ": "
          GA.PromptDefault (Just pDefault) -> fmt "" +| prompt |+ " {" +| pDefault |+ "}: "
          GA.PromptDefault Nothing -> fmt "" +| prompt |+ ": "

    answer <-
      case pType of
        GA.PromptOneTime -> askPrompt
        GA.PromptDefault Nothing -> until (not . null) askPrompt
        GA.PromptDefault (Just pDefault) -> do
          answer <- askPrompt
          if null answer
            then return pDefault
            else return answer
    return $ next answer
  GA.FormatInfo content next -> do
    return $ next $ colored Green Normal content
  GA.FormatTextRed content next -> do
    return $ next $ colored Cyan Bold content
  GA.FormatCommand content next -> do
    return $ next $ colored Green Normal "==>>" <> " " <> colored Blue Bold content
  GA.PrintText content next -> do
    pText content
    return next
  GA.PrintTextLn content next -> do
    pTextLn content
    return next

colored :: Color -> FontStyle -> Text -> Text
colored color style content =
  fmt "\x1b[" +|| fontStyleCode style ||+ ";" +|| colorCode color ||+ "m" +| content |+ "\x1b[0m"

data Color
  = Red
  | Green
  | Blue
  | Purple
  | Cyan

colorCode :: Color -> Int
colorCode Red = 31
colorCode Green = 32
colorCode Blue = 34
colorCode Purple = 35
colorCode Cyan = 36

data FontStyle
  = Normal
  | Bold

fontStyleCode :: FontStyle -> Int
fontStyleCode Normal = 0
fontStyleCode Bold = 1
