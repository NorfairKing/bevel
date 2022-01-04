{-# OPTIONS_GHC -fno-warn-orphans #-}

module Bevel.API.Data.Gen where

import Bevel.API.Data
import Bevel.API.Server.Data.Gen ()
import Data.GenValidity
import Data.GenValidity.Appendful ()
import Data.GenValidity.Text ()

instance GenValid RegistrationForm where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid LoginForm where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid SyncRequest where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid SyncResponse where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
