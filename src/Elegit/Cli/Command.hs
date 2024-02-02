module Elegit.Cli.Command where

import Universum

data CloneRepositoryData = CloneRepositoryData
  { _crdOptions :: NonEmpty Text
  }
  deriving (Eq, Show)

data ElegitCommand
  = ShowWorkCommand
  | AcquireRepositoryCommand
  | InitRepositoryCommand
  | CloneRepositoryCommand CloneRepositoryData
  deriving (Eq, Show)
