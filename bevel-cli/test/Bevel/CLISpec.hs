module Bevel.CLISpec (spec) where

import Bevel.API.Data
import Bevel.API.Server.Data
import Bevel.API.Server.TestUtils
import Bevel.CLI
import qualified Data.Text as T
import Path
import Path.IO
import Servant.Client
import System.Environment
import System.Exit
import System.Process.Typed (closed, proc, runProcess, setStdout, setWorkingDir)
import qualified System.Process.Typed as Process (setEnv)
import Test.Syd
import Test.Syd.Validity

-- Sequential because these tests set process-global arguments and environment.
spec :: Spec
spec = sequential $ do
  describe "migrate" $
    it "sets up a database that bevel-gather can write to" $
      withSystemTempDir "bevel-migrate" $ \tdir -> do
        dataDir <- resolveDir tdir "data"
        dbFile <- resolveFile dataDir "history.sqlite3"
        withArgs ["migrate", "--database", fromAbsFile dbFile] bevelCLI
        let pc =
              setStdout closed $
                Process.setEnv [("BEVEL_DATABASE", fromAbsFile dbFile)] $
                  setWorkingDir (fromAbsDir tdir) $
                    proc "bevel-gather" ["echo hi"]
        ec <- runProcess pc
        ec `shouldBe` ExitSuccess

  serverSpec $
    describe "Bevel CLI" $
      it "'just works'" $
        \cenv -> forAllValid $ \rf -> withSystemTempDir "bevel-cli" $ \tdir -> do
          dbFile <- resolveFile tdir "bevel-client.sqlite3"
          let testBevel args = do
                setEnv "BEVEL_SERVER_URL" $ showBaseUrl $ baseUrl cenv
                setEnv "BEVEL_USERNAME" $ T.unpack $ usernameText $ registrationFormUsername rf
                setEnv "BEVEL_PASSWORD" $ T.unpack $ registrationFormPassword rf
                setEnv "BEVEL_DATABASE" $ fromAbsFile dbFile
                withArgs args bevelCLI
          testBevel ["register"]
          testBevel ["login"]
          testBevel ["sync"]
