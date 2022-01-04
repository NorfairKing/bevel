{-# LANGUAGE RecordWildCards #-}

module Bevel.CLI.Commands.Register where

import Bevel.CLI.Commands.Import

register :: C ()
register = withClient $ \cenv -> do
  registrationFormUsername <- getEnvUsername
  registrationFormPassword <- getEnvPassword
  let rf = RegistrationForm {..}
  NoContent <- runClientOrDie cenv $ postRegister bevelClient rf
  pure ()
