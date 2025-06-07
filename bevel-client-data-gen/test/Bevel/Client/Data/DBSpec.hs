module Bevel.Client.Data.DBSpec where

import Bevel.API.Server.Data.Gen ()
import Bevel.Client.Data.DB
import Test.Syd
import Test.Syd.Persistent.Sqlite
import Test.Syd.Validity

spec :: Spec
spec = do
  sqliteMigrationSucceedsSpec "test_resources/migration.sql" automaticClientMigrations
  describe "makeUnsyncedClientCommand" $
    it "roundtrips with clientMakeCommand" $
      forAllValid $ \command ->
        clientMakeCommand (makeUnsyncedClientCommand command) `shouldBe` command
