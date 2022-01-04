{-# OPTIONS_GHC -fno-warn-orphans #-}

module Bevel.Data.Command.Gen where

import Bevel.Data.Command
import Data.GenValidity
import Data.GenValidity.Path ()
import Data.GenValidity.Text ()

instance GenValid Command where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
