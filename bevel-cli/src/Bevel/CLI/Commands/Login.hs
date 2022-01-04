module Bevel.CLI.Commands.Login where

import Bevel.CLI.Commands.Import

login :: C ()
login = withClient $ \cenv -> withLogin cenv $ \_ ->
  pure ()
