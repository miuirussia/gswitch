{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Config.Env where

import Data.Text
import Control.Lens (makeFields)
import Env

data Config = Config
  { _configPrivateKey :: Text
  , _configHost       :: String
  , _configPort       :: Int
  , _configPath       :: String
  } deriving (Show)

makeFields ''Config

getEnvironmentConfig :: IO Config
getEnvironmentConfig = Env.parse (header "gswitch") . prefixed "GSWITCH_" $
                      Config <$> var (str <=< nonempty) "GITLAB_PRIVATE_KEY" (help "GitLab private key")
                             <*> var (str <=< nonempty) "GITLAB_HOST" (help "host (eg \"gitlab.com\")")
                             <*> var (auto <=< nonempty) "GITLAB_PORT" (help "port (eg 443)")
                             <*> var (str <=< nonempty) "GITLAB_PATH" (help "path (eg \"/api/v4\")")
