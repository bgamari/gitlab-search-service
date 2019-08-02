{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}

import Data.Void
import Data.Proxy
import Data.Maybe
import Control.Monad.IO.Class
import Control.Monad
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import Servant.HTML.Lucid
import Lucid (Html)
import Servant
import Servant.Server
import Network.Wai.Handler.Warp

import Index

newtype Project = Project {getProject :: String}
                deriving (Show)

data Req = IssueQuery (Maybe Project) Int           -- ^ @grp/proj#123@
         | MRQuery (Maybe Project) Int              -- ^ @grp/proj!123@
         | IssueSearchQuery (Maybe Project) String  -- ^ @grp/proj# search terms@
         | MRSearchQuery (Maybe Project) String     -- ^ @grp/proj! search terms@
         | MilestoneQuery (Maybe Project) String    -- ^ @grp/proj%milestone name@
         | ProjectQuery Project                     -- ^ @grp/proj@
         | SearchQuery (Maybe Project) String       -- ^ @grp/proj search terms@
         deriving (Show)

parseQuery :: Parsec Void String Req
parseQuery =
  msum 
  $ map try
  [ IssueQuery <$> maybeProj <* char '#' <*> decimal
  , IssueQuery <$> maybeProj <* string " issue " <*> decimal
  , MRQuery <$> maybeProj <* char '!' <*> decimal
  , MRQuery <$> maybeProj <* string " mr " <*> decimal
  , IssueSearchQuery <$> maybeProj <* char '#' <*> searchTerms
  , MRSearchQuery <$> maybeProj <* char '!' <*> searchTerms
  , MilestoneQuery <$> maybeProj <* char '%' <*> searchTerms
  , ProjectQuery <$> project
  , SearchQuery <$> maybeProj <* space1 <*> searchTerms
  ]
  where
    maybeProj = optional project
    project = do
      group1 <- letterChar
      group <- some alphaNumChar
      char '/'
      proj <- some alphaNumChar
      pure $ Project $ group1:group++"/"++proj
    searchTerms = some anySingle

type Get303 (cts :: [*]) (hs :: [*]) a = Verb 'GET 303 cts (Headers (Header "Location" String ': hs) a)

type API =
  ("search" :> QueryParam' '[ Required ] "q" String :> Get303 '[ FormUrlEncoded ] '[] NoContent)
  :<|> Get '[ HTML ] (Html ())

server :: Server API
server = serveQuery :<|> serveHome
  where
    serveHome = pure indexPage
    serveQuery q = do
      liftIO $ print q
      req <- either (fail . show) pure $ parse parseQuery "query string" q
      liftIO $ print req
      let url = case req of
                  IssueQuery p n -> gitlabUrl </> defProj p </> "issues" </> show n
                  MRQuery p n -> gitlabUrl </> defProj p </> "merge_requests" </> show n
                  ProjectQuery p -> gitlabUrl </> getProject p
                  MilestoneQuery p m -> gitlabUrl </> defProj p </> "milestones?utf8=✓&search_title=" <> m
                  IssueSearchQuery p s -> searchUrl "issues" p s
                  MRSearchQuery p s -> searchUrl "merge_requests" p s
                  SearchQuery p s -> searchUrl "" p s

          searchUrl scope proj query = gitlabUrl </> "search?utf8=✓&scope=" <> scope <> "&search=" <> query
          gitlabUrl = "https://gitlab.haskell.org"
          defProj = getProject . fromMaybe defaultProject
          defaultProject = Project "ghc/ghc"
          a </> b = a ++ "/" ++ b

      return $ addHeader url NoContent

app :: Application
app = serve (Proxy @API) server

main :: IO ()
main = Network.Wai.Handler.Warp.run 7003 app
