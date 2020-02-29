{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
module Api.GitLab ( Branch(..)
                  , Ref (..)
                  , BranchCreateRequest (..)
                  , MergeRequestRequest (..)
                  , getIssueInfo
                  , createBranch
                  , createMergeRequest
                  , createMergeRequestReq
                  ) where

import Data.Aeson
import Deriving.Aeson
import Data.Proxy
import Data.Text

import Servant.API
import Servant.Client

newtype Branch = Branch { unBranch :: Text }
  deriving (Show, Generic)

instance FromJSON Branch

instance ToJSON Branch where
  toJSON =
    toJSON . unBranch
  toEncoding =
    toEncoding . unBranch


newtype Ref = Ref { unRef :: Text }
  deriving (Show, Generic)

instance FromJSON Ref

instance ToJSON Ref where
  toJSON =
    toJSON . unRef
  toEncoding =
    toEncoding . unRef

data BranchCreateRequest = BranchCreateRequest
  { bcRequestBranch :: Branch
  , bcRequestRef    :: Ref
  }
  deriving Generic
  deriving ToJSON via CustomJSON '[FieldLabelModifier (StripPrefix "bcRequest", CamelToSnake)] BranchCreateRequest

data MergeRequestRequest = MergeRequestRequest
  { mrRequestSourceBranch          :: Branch
  , mrRequestTargetBranch          :: Branch
  , mrRequestTitle                 :: Text
  , mrRequestAssigneeId            :: Maybe Int
  , mrRequestAssigneeIds           :: Maybe [Int]
  , mrRequestDescription           :: Maybe Text
  , mrRequestTargetProjectId       :: Maybe Int
  , mrRequestLabels                :: Maybe Text
  , mrRequestMilestoneId           :: Maybe Text
  , mrRequestRemoveSourceBranch    :: Maybe Bool
  , mrRequestAllowCollaboration    :: Maybe Bool
  , mrRequestAllowMaintainerToPush :: Maybe Bool
  , mrRequestSquash                :: Maybe Bool
  }
  deriving Generic
  deriving ToJSON via CustomJSON '[OmitNothingFields, FieldLabelModifier (StripPrefix "mrRequest", CamelToSnake)] MergeRequestRequest

data IssueResponse = IssueResponse
  { iResponseId :: Int
  , iResponseTitle :: Text
  } deriving (Show, Generic)

instance FromJSON IssueResponse where
  parseJSON = withObject "IssueResponse" $ \o -> do
    iResponseId <- o .: "iid"
    iResponseTitle <- o .: "title"
    pure IssueResponse { .. }

data BranchResponse = BranchResponse
  { bResponseName     :: Branch
  , bResponseCommitId :: Ref
  }

instance FromJSON BranchResponse where
  parseJSON = withObject "BranchResponse" $ \o -> do
    bResponseName <- o .: "name"
    commit <- o .: "commit"
    bResponseCommitId <- commit .: "id"
    pure BranchResponse { .. }

data MergeRequestResponse = MergeRequestResponse
  { mrResponseId           :: Int
  , mrResponseTitle        :: Int
  , mrResponseDescription  :: Text
  , mrResponseTargetBranch :: Branch
  , mrResponseSourceBranch :: Branch
  }
  deriving Generic
  deriving FromJSON via CustomJSON '[FieldLabelModifier (StripPrefix "mergeRequest", CamelToSnake)] MergeRequestResponse


type API =
          ("projects" :> Capture "id" Text :> "issues" :> Capture "issue_iid" Int :> Get '[JSON] IssueResponse)
     :<|> ("projects" :> Capture "id" Text :> "repository" :> "branches" :> ReqBody '[JSON] BranchCreateRequest :> Post '[JSON] BranchResponse)
     :<|> ("projects" :> Capture "id" Text :> "merge_requests" :> ReqBody '[JSON] MergeRequestRequest :> Post '[JSON] MergeRequestResponse)

api :: Proxy API
api = Proxy

getIssueInfo :: Text -> Int -> ClientM IssueResponse
createBranch :: Text -> BranchCreateRequest -> ClientM BranchResponse
createMergeRequest :: Text -> MergeRequestRequest -> ClientM MergeRequestResponse

getIssueInfo :<|> createBranch :<|> createMergeRequest = client api

createMergeRequestReq :: Branch -> Branch -> Text -> Text -> MergeRequestRequest
createMergeRequestReq sourceBranch targetBranch title description = MergeRequestRequest
  { mrRequestSourceBranch = sourceBranch
  , mrRequestTargetBranch = targetBranch
  , mrRequestTitle = title
  , mrRequestDescription = Just description
  , mrRequestAssigneeId = Nothing
  , mrRequestAssigneeIds = Nothing
  , mrRequestTargetProjectId = Nothing
  , mrRequestLabels = Nothing
  , mrRequestMilestoneId = Nothing
  , mrRequestRemoveSourceBranch = Just True
  , mrRequestAllowCollaboration = Nothing
  , mrRequestAllowMaintainerToPush = Nothing
  , mrRequestSquash = Nothing
  }

