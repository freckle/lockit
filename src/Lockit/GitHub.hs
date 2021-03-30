module Lockit.GitHub
    ( Issue(..)
    , issueIsFresh

    -- * Effects
    , fetchClosedIssues
    , lockIssue

    -- * Re-exports
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

issueIsFresh :: UTCTime -> Issue -> Bool
issueIsFresh = undefined

fetchClosedIssues :: Name Organization -> Name Repo -> m [Issue]
fetchClosedIssues = undefined

lockIssue :: Issue -> m ()
lockIssue = undefined
