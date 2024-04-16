module Bevel.Client.Data.DBSpec where

import Bevel.API.Server.Data.Gen ()
import Bevel.Client.Data.DB
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  describe "makeUnsyncedClientCommand" $
    it "roundtrips with clientMakeCommand" $
      forAllValid $ \command ->
        clientMakeCommand (makeUnsyncedClientCommand command) `shouldBe` command
