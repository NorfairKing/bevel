module Bevel.API.Server.Handler.AuthSpec (spec) where

import Bevel.API
import Bevel.API.Data
import Bevel.API.Data.Gen ()
import Bevel.API.Server.TestUtils
import Bevel.Client
import Network.HTTP.Types as HTTP
import Test.QuickCheck
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = serverSpec $ do
  describe "PostRegister" $
    it "does not crash" $
      \cenv ->
        forAllValid $ \rf -> do
          NoContent <- testClientOrErr cenv $ postRegister bevelClient rf
          pure ()
  describe "PostRegister" $ do
    it "fails before registration" $ \cenv ->
      forAllValid $ \lf -> do
        errOrRes <- testClient cenv $ postLogin bevelClient lf
        case errOrRes of
          Left err -> case err of
            FailureResponse _ resp | responseStatusCode resp == HTTP.unauthorized401 -> pure ()
            _ -> expectationFailure $ "Should have errored with code 401, got this instead: " <> show err
          _ -> expectationFailure "Should have errored"
    it "shows no difference between a login failure for a user that exists and a user that doesn't exist" $ \cenv ->
      forAllValid $ \un1 ->
        forAll (genValid `suchThat` (/= un1)) $ \un2 ->
          forAllValid $ \pw1 ->
            forAll (genValid `suchThat` (/= pw1)) $ \pw2 -> do
              -- Sign up user 1 but not user 2
              NoContent <- testClientOrErr cenv $ postRegister bevelClient $ RegistrationForm {registrationFormUsername = un1, registrationFormPassword = pw1}
              errOrRes1 <- testClient cenv $ postLogin bevelClient $ LoginForm {loginFormUsername = un1, loginFormPassword = pw2}
              errOrRes2 <- testClient cenv $ postLogin bevelClient $ LoginForm {loginFormUsername = un2, loginFormPassword = pw2}
              () <$ errOrRes1 `shouldBe` () <$ errOrRes2
    it "succeeds after registration" $ \cenv ->
      forAllValid $ \rf -> do
        _ <- testClientOrErr cenv $ do
          NoContent <- postRegister bevelClient rf
          postLogin bevelClient $ registrationFormToLoginForm rf
        pure ()
