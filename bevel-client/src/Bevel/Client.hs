module Bevel.Client
  ( module Bevel.Client,
    module Bevel.API,
    module X,
  )
where

import Bevel.API
import Servant.API as X
import Servant.Auth.Client as X
import Servant.Client as X
import Servant.Client.Generic

bevelClient :: BevelRoutes (AsClientT ClientM)
bevelClient = genericClient
