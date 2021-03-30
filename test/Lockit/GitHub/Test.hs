{-# LANGUAGE UndecidableInstances #-}

module Lockit.GitHub.Test
    ( IssueMap
    , HasIssueMap(..)
    , makeIssueMap
    , lookupIssue
    , MockGitHubIssues(..)
    ) where

import RIO

import Control.Lens (use)
import Control.Monad.State
import Lockit.GitHub
import qualified RIO.HashMap as HashMap

newtype IssueMap = IssueMap
    { unIssueMap :: HashMap (Id Issue) Issue
    }

unL :: Lens' IssueMap (HashMap (Id Issue) Issue)
unL = lens unIssueMap $ \x y -> x { unIssueMap = y }

class HasIssueMap env where
    issueMapL :: Lens' env IssueMap

instance HasIssueMap IssueMap where
    issueMapL = id

makeIssueMap :: [Issue] -> IssueMap
makeIssueMap = IssueMap . HashMap.fromList . map (issueId &&& id)

lookupIssue :: Id Issue -> IssueMap -> Maybe Issue
lookupIssue x = HashMap.lookup x . unIssueMap

newtype MockGitHubIssues m a = MockGitHubIssues
    { unMockGitHubIssues :: m a
    }
    deriving
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadState env
        )

instance (MonadState env m, HasIssueMap env)
    => MonadGitHub (MockGitHubIssues m) where
    fetchClosedIssues _ _ = use $ issueMapL . unL . to HashMap.elems
    lockIssue issueToLock = modify $ over (issueMapL . unL) $ HashMap.map
        (\issue -> if issue == issueToLock
            then issue { issueLocked = True }
            else issue
        )
