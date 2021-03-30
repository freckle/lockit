module Lockit.GitHub.Test
    ( IssueMap
    , makeIssueMap
    , lookupIssue
    , issueMapIssues
    , issueMapUpdate
    ) where

import RIO

import Lockit.GitHub
import qualified RIO.HashMap as HashMap

newtype IssueMap = IssueMap
    { unIssueMap :: HashMap (Id Issue) Issue
    }

makeIssueMap :: [Issue] -> IssueMap
makeIssueMap = IssueMap . HashMap.fromList . map (issueId &&& id)

lookupIssue :: Id Issue -> IssueMap -> Maybe Issue
lookupIssue x = HashMap.lookup x . unIssueMap

issueMapIssues :: IssueMap -> [Issue]
issueMapIssues = HashMap.elems . unIssueMap

issueMapUpdate :: (Issue -> Issue) -> IssueMap -> IssueMap
issueMapUpdate f = IssueMap . HashMap.map f . unIssueMap
