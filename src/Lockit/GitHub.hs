module Lockit.GitHub
    ( Issue(..)
    , mkIssueId
    , issueIsFresh
    , fromIssuesResponse

    -- * Effects
    , MonadGitHub(..)

    -- * Re-exports
    , Id
    , Name
    , Owner
    , mkOwnerName
    , Repo
    , mkRepoName
    ) where

import RIO

import qualified GitHub
import GitHub.Data hiding (Issue(..))
import Lockit.Time
import qualified RIO.Vector as V

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
    | Just closed <- issueClosedAt issue, closed <= cutoff = False
    | otherwise = True

fromIssuesResponse :: Vector GitHub.Issue -> [Issue]
fromIssuesResponse = V.toList . fmap fromIssue
  where
    fromIssue :: GitHub.Issue -> Issue
    fromIssue issue = Issue
        { issueId = mkIssueId $ untagId $ GitHub.issueId issue
        , issueLocked = False -- github package lacks API
        , issueClosedAt = GitHub.issueClosedAt issue
        }

class Monad m => MonadGitHub m where
    fetchClosedIssues :: Name Owner -> Name Repo -> m [Issue]
    lockIssue :: Issue -> m ()
