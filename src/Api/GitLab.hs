{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Api.GitLab where

import Data.Aeson
import Deriving.Aeson
import Data.Proxy
import Data.Text
import Control.Lens (makeFields)

import Servant.API
import Servant.API.Flatten
import Servant.Client

newtype Branch = Branch { unBranch :: Text }
  deriving (Show, Generic)

instance FromJSON Branch where
  parseJSON = withText "Branch" $ \t -> do
    pure (Branch t)

instance ToJSON Branch where
  toJSON =
    toJSON . unBranch
  toEncoding =
    toEncoding . unBranch

newtype Ref = Ref { unRef :: Text }
  deriving (Show, Generic)

instance FromJSON Ref where
  parseJSON = withText "Ref" $ \t -> do
    pure (Ref t)

instance ToJSON Ref where
  toJSON =
    toJSON . unRef
  toEncoding =
    toEncoding . unRef

data BranchCreateRequest = BranchCreateRequest
  { _branchCreateRequestBranch :: Branch
  , _branchCreateRequestRef    :: Ref
  }
  deriving (Show, Generic)
  deriving ToJSON via CustomJSON '[FieldLabelModifier (StripPrefix "_branchCreateRequest", CamelToSnake)] BranchCreateRequest

makeFields ''BranchCreateRequest

data MergeRequestRequest = MergeRequestRequest
  { _mergeRequestRequestSourceBranch          :: Branch
  , _mergeRequestRequestTargetBranch          :: Branch
  , _mergeRequestRequestTitle                 :: Text
  , _mergeRequestRequestAssigneeId            :: Maybe Int
  , _mergeRequestRequestAssigneeIds           :: Maybe [Int]
  , _mergeRequestRequestDescription           :: Maybe Text
  , _mergeRequestRequestTargetProjectId       :: Maybe Int
  , _mergeRequestRequestLabels                :: Maybe Text
  , _mergeRequestRequestMilestoneId           :: Maybe Text
  , _mergeRequestRequestRemoveSourceBranch    :: Maybe Bool
  , _mergeRequestRequestAllowCollaboration    :: Maybe Bool
  , _mergeRequestRequestAllowMaintainerToPush :: Maybe Bool
  , _mergeRequestRequestSquash                :: Maybe Bool
  }
  deriving (Show, Generic)
  deriving ToJSON via CustomJSON '[OmitNothingFields, FieldLabelModifier (StripPrefix "_mergeRequestRequest", CamelToSnake)] MergeRequestRequest

makeFields ''MergeRequestRequest

data IssueResponse = IssueResponse
  { _issueResponseId :: Int
  , _issueResponseTitle :: Text
  } deriving (Show, Generic)

makeFields ''IssueResponse

instance FromJSON IssueResponse where
  parseJSON = withObject "IssueResponse" $ \o -> do
    _issueResponseId <- o .: "iid"
    _issueResponseTitle <- o .: "title"
    pure IssueResponse { .. }

data BranchResponse = BranchResponse
  { _branchResponseName     :: Branch
  , _branchResponseCommitId :: Ref
  } deriving (Show, Generic)

makeFields ''BranchResponse

instance FromJSON BranchResponse where
  parseJSON = withObject "BranchResponse" $ \o -> do
    _branchResponseName <- o .: "name"
    commit <- o .: "commit"
    _branchResponseCommitId <- commit .: "id"
    pure BranchResponse { .. }

data MergeRequestResponse = MergeRequestResponse
  { _mergeRequestResponseId           :: Int
  , _mergeRequestResponseTitle        :: Text
  , _mergeRequestResponseDescription  :: Text
  , _mergeRequestResponseTargetBranch :: Branch
  , _mergeRequestResponseSourceBranch :: Branch
  }
  deriving (Show, Generic)
  deriving FromJSON via CustomJSON '[FieldLabelModifier (StripPrefix "_mergeRequestResponse", CamelToSnake)] MergeRequestResponse

makeFields ''MergeRequestResponse

createMergeRequestReq :: Branch -> Branch -> Text -> Text -> MergeRequestRequest
createMergeRequestReq a b c d = MergeRequestRequest
  { _mergeRequestRequestSourceBranch = a
  , _mergeRequestRequestTargetBranch = b
  , _mergeRequestRequestTitle = c
  , _mergeRequestRequestDescription = Just d
  , _mergeRequestRequestAssigneeId = Nothing
  , _mergeRequestRequestAssigneeIds = Nothing
  , _mergeRequestRequestTargetProjectId = Nothing
  , _mergeRequestRequestLabels = Nothing
  , _mergeRequestRequestMilestoneId = Nothing
  , _mergeRequestRequestRemoveSourceBranch = Just True
  , _mergeRequestRequestAllowCollaboration = Nothing
  , _mergeRequestRequestAllowMaintainerToPush = Nothing
  , _mergeRequestRequestSquash = Nothing
  }

type PrivateApi = ("issues" :> Capture "issue_iid" Int :> Get '[JSON] IssueResponse)
             :<|> ("repository" :> "branches" :> ReqBody '[JSON] BranchCreateRequest :> Post '[JSON] BranchResponse)
             :<|> ("merge_requests" :> ReqBody '[JSON] MergeRequestRequest :> Post '[JSON] MergeRequestResponse)

type API = Header "PRIVATE-TOKEN" Text :> "projects" :> Capture "id" Int :> PrivateApi

api :: Proxy API
api = Proxy

getIssueInfo :: Maybe Text -> Int -> Int -> ClientM IssueResponse
createBranch :: Maybe Text -> Int -> BranchCreateRequest -> ClientM BranchResponse
createMergeRequest :: Maybe Text -> Int -> MergeRequestRequest -> ClientM MergeRequestResponse

getIssueInfo :<|> createBranch :<|> createMergeRequest = client (flatten api)
