{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Bevel.API.Server.Data.Gen where

import Bevel.API.Server.Data
import Bevel.Data.Gen ()
import Data.GenValidity
import Data.GenValidity.Persist ()
import Data.GenValidity.Text ()

instance GenValid Username where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally
