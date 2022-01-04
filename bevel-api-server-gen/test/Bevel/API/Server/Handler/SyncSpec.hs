module Bevel.API.Server.Handler.SyncSpec (spec) where

import Bevel.API
import Bevel.API.Data.Gen ()
import Bevel.API.Server.TestUtils
import Bevel.Client
import Bevel.Data.Gen ()
import Test.Hspec
import Test.Validity

spec :: Spec
spec = serverSpec $
  describe "PostSync" $
    it "does not crash" $
      \cenv -> forAllValid $ \req -> withAnyNewUser cenv $ \token -> do
        _ <- testClientOrErr cenv $ postSync bevelClient token req
        pure ()
