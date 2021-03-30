module Lockit.GitHub
    ( Issue(..)
    , mkIssueId
    , issueIsFresh

    -- * Effects
    , fetchClosedIssues
    , lockIssue

    -- * Re-exports
    , Id
    , Name
    , Owner
    , mkOwnerName
    , Repo
    , mkRepoName
    ) where

import RIO

import GitHub.Data hiding (Issue(..))
import Lockit.Time

data Issue = Issue
    { issueId :: Id Issue
    , issueLocked :: Bool
    , issueClosedAt :: Maybe UTCTime
    }

instance Eq Issue where
    (==) = (==) `on` issueId

mkIssueId :: Int -> Id Issue
mkIssueId = mkId Proxy

issueIsFresh :: UTCTime -> Issue -> Bool
issueIsFresh cutoff issue
    | issueLocked issue = False
    | Just closed <- issueClosedAt issue, closed > cutoff = False
    | otherwise = True

fetchClosedIssues
    :: Applicative m => Name Owner -> Name Repo -> m [Issue]
fetchClosedIssues _ _ = pure []

lockIssue :: Applicative m => Issue -> m ()
lockIssue _ = pure ()
