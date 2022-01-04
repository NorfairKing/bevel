{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Bevel.API.Server.Data.Gen where

import Bevel.API.Server.Data
import Bevel.Data.Gen ()
import Control.Monad
import qualified Data.ByteString as SB
import Data.GenValidity
import Data.GenValidity.Persist ()
import Data.GenValidity.Text ()
import Data.Password.Bcrypt
import Test.QuickCheck

instance GenValid (Salt a) where
  genValid = Salt <$> (SB.pack <$> replicateM 32 (choose (0, 255)))
  shrinkValid _ = [] -- No use

instance GenValid Password where
  genValid = mkPassword <$> genValid
  shrinkValid _ = [] -- No use

instance GenValid (PasswordHash Bcrypt) where
  genValid = hashPasswordWithSalt 4 <$> genValid <*> (mkPassword <$> genValid)
  shrinkValid _ = [] -- No use

instance GenValid Username where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid User where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid ServerAppendfulThing where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally
