module Compile.Variable where

import qualified Data.Text as Text

data Variable
  = Captured Int
  | External Text.Text
  deriving (Eq, Ord, Show)
