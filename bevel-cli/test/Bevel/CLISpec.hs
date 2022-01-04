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
import Test.Hspec
import Test.Validity

spec :: Spec
spec = serverSpec $
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
