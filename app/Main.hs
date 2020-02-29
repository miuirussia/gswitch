module Main where

import Servant.Client
import Control.Lens ((^.))
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Client (newManager)

import qualified Data.Char as C
import qualified Data.Text as T
import qualified Api.GitLab as G

main :: IO ()
main = do
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
          print branch
      where
        branchName = "feature/" <> T.pack (show (issue ^. G.id)) <> "-" <> T.filter isValidBranchSymbol (issue ^. G.title)
        sourceRef = "master"
        isValidBranchSymbol c = C.isAscii c && C.isLetter c
  where
    privateKey = "<T>"
    projectId = 17202494
    issueIdN = 1
    baseUrl = BaseUrl Https "gitlab.com" 443 "/api/v4"
