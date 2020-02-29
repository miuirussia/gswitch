module Main where

import Servant.Client
import Control.Lens ((^.))
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Client (newManager)


import qualified Data.Char as C
import qualified Data.Text as T
import qualified Config.Env as CE
import qualified Api.GitLab as G

main :: IO ()
main = do
  env <- CE.getEnvironmentConfig
  let privateKey = env ^. CE.privateKey
  let baseUrl = BaseUrl Https (env ^. CE.host) (env ^. CE.port) (env ^. CE.path)

  manager <- newManager tlsManagerSettings
  let clientEnv = mkClientEnv manager baseUrl
  issueClient <- runClientM (G.getIssueInfo (Just privateKey) projectId issueIdN) clientEnv
  case issueClient of
    Left err -> putStrLn $ "Error: " ++ show err
    Right issue -> do
      branchClient <- runClientM (G.createBranch (Just privateKey) projectId (G.BranchCreateRequest (G.Branch branchName) (G.Ref sourceRef))) clientEnv
      case branchClient of
        Left err -> putStrLn $ "Error: " ++ show err
        Right branch -> do
          mergeRequestClient <- runClientM (G.createMergeRequest (Just privateKey) projectId (G.createMergeRequestReq (branch ^. G.name) (G.Branch sourceRef) mergeRequestName mergeRequestDescription)) clientEnv
          print mergeRequestClient
      where
        issueIdS = T.pack (show (issue ^. G.id))
        branchName = "feature/" <> issueIdS <> "-" <> T.filter isValidBranchSymbol (issue ^. G.title)
        mergeRequestName = "feature ui #" <> issueIdS <> ": " <> issue ^. G.title
        mergeRequestDescription = "Closes #" <> issueIdS
        sourceRef = "master"
        isValidBranchSymbol c = C.isAscii c && C.isLetter c
  where
    projectId = 17202494
    issueIdN = 1
