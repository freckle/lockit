{-# LANGUAGE UndecidableInstances #-}

module Lockit.GitHub
    ( Issue(..)
    , mkIssueId
    , issueIsFresh
    , fromIssuesResponse

    -- * Effects
    , MonadGitHub(..)

    -- * Implementations
    , HasGitHubAuth(..)
    , ActualGitHub(..)

    -- * Re-exports
    , Id
    , Name
    , Owner
    , mkOwnerName
    , Repo
    , mkRepoName
    ) where

import RIO

import GitHub hiding (Issue(..))
import qualified GitHub
import Lockit.Time
import qualified RIO.Vector as V

data Issue = Issue
    { issueId :: Id Issue
    , issueLocked :: Bool
    , issueClosedAt :: Maybe UTCTime
    }
    deriving stock Show

instance Eq Issue where
    (==) = (==) `on` issueId

instance Display Issue where
    display = displayShow

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

class HasGitHubAuth env where
    githubAuthL :: Lens' env GitHub.Auth

instance HasGitHubAuth GitHub.Auth where
    githubAuthL = id

newtype ActualGitHub m a = ActualGitHub
    { unActualGitHub :: m a
    }
    deriving
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadReader env
        )

instance (MonadIO m, MonadReader env m, HasGitHubAuth env, HasLogFunc env)
    => MonadGitHub (ActualGitHub m) where
    fetchClosedIssues org repo = do
        auth <- view githubAuthL
        result <- liftIO $ github auth $ issuesForRepoR
            org
            repo
            stateClosed
            FetchAll
        either throwIO (pure . fromIssuesResponse) result

    -- Haskell github package lacks API
    lockIssue issue = logInfo $ "Locking Issue: " <> display issue
