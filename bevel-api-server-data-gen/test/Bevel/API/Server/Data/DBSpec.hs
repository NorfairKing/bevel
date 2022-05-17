module Bevel.API.Server.Data.DBSpec where

import Bevel.API.Server.Data.DB
import Bevel.API.Server.Data.Gen ()
import Test.Syd
import Test.Syd.Persistent.Sqlite
import Test.Syd.Validity

spec :: Spec
spec = do
  sqliteMigrationSucceedsSpec "test_resources/migration.sql" serverMigration
  describe "makeUnsyncedServerCommand" $
    it "roundtrips with serverMakeCommand" $
      forAllValid $ \userId ->
        forAllValid $ \command ->
          serverMakeCommand (makeServerCommand userId command) `shouldBe` command
