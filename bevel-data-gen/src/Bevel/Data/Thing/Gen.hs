{-# OPTIONS_GHC -fno-warn-orphans #-}

module Bevel.Data.Thing.Gen where

import Bevel.Data.Thing
import Data.GenValidity

instance GenValid Thing where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
