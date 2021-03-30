module Lockit
    ( run
    ) where

import RIO

import Lockit.GitHub
import Lockit.Time

run :: MonadGitHub m => Name Owner -> Name Repo -> UTCTime -> m ()
run org repo cutoff = do
    issues <- fetchClosedIssues org repo
    traverse_ lockIssue $ filter (not . issueIsFresh cutoff) issues
