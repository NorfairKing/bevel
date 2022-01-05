{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Bevel.Client.Data.Gen where

import Bevel.Client.Data
import Bevel.Data.Gen ()
import Data.GenValidity
import Data.GenValidity.Persist ()
import Data.GenValidity.Text ()

instance GenValid ClientCommand where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally
