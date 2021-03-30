module Lockit.App
    ( App
    , loadApp
    , AppT
    , runAppT
    ) where

import RIO

import qualified Data.ByteString.Char8 as BS8
import GitHub
import System.Environment

newtype App = App
    { appGitHubAuth :: Auth
    }

loadApp :: IO App
loadApp = do
    token <- getEnv "GITHUB_TOKEN"
    pure App { appGitHubAuth = OAuth $ BS8.pack token }

newtype AppT m a = AppT
    { unAppT :: ReaderT App m a
    }
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadReader App
        )

runAppT :: App -> AppT m a -> m a
runAppT app f = runReaderT (unAppT f) app
