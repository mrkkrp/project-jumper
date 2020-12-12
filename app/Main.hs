{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Monad (forM)
import Data.List (intercalate, sortOn)
import Data.List.NonEmpty (NonEmpty (..), groupWith)
import Data.Ord (Down (..))
import Data.Ratio (Ratio, (%))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Version (showVersion)
import Development.GitRev
import Options.Applicative
import Path
import Path.IO
import Paths_project_jumper (version)
import System.Exit (exitFailure)
import System.FilePath (dropTrailingPathSeparator)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  Opts {..} <- execParser optsParserInfo
  projectsRoot <- getProjectsRoot
  projects <- listProjects projectsRoot
  print optKeyword
  print projects
  target <-
    selectMatch optKeyword projects >>= \case
      x :| [] -> return x
      choices -> chooseMatch choices
  projectDir projectsRoot target >>= putStrLn . fromAbsDir

-- | The root of the project directory.
newtype ProjectsRoot = ProjectsRoot (Path Abs Dir)

-- | The directory of a project.
data Project = Project
  { -- | Project owner.
    projectOwner :: Text,
    -- | Project name.
    projectName :: Text
  }
  deriving (Show)

-- | Locate project directory for current user.
getProjectsRoot :: IO ProjectsRoot
getProjectsRoot = do
  homeDir <- getHomeDir
  return $ ProjectsRoot (homeDir </> $(mkRelDir "projects"))

-- | List available projects.
listProjects :: ProjectsRoot -> IO [Project]
listProjects (ProjectsRoot root) = do
  (ownerDirs, _) <- listDirRel root
  fmap concat . forM ownerDirs $ \owner -> do
    (names, _) <- listDirRel (root </> owner)
    let relDirToText = T.pack . dropTrailingPathSeparator . toFilePath
        ownerText = relDirToText owner
        toProject = Project ownerText . relDirToText
    return (toProject <$> names)

-- | Select a matching project(s).
selectMatch ::
  -- | The keyword
  Text ->
  -- | Projects to choose from
  [Project] ->
  -- | Either a collection of close-enough projects or a definitive match
  IO (NonEmpty Project)
selectMatch keyword projects = do
  case groupWith fst
    . sortOn (Down . fst)
    $ fmap assignScore projects of
    [] -> do
      hPutStrLn stderr "No matches found."
      exitFailure
    (matches : _) -> return (snd <$> matches)
  where
    assignScore project =
      (score keyword (projectName project), project)

-- | Calculate the similarity score between the keyword and a project name.
score ::
  -- | The keyword
  Text ->
  -- | Project name
  Text ->
  -- | The score
  Ratio Int
score keyword name =
  if k `T.isInfixOf` n
    then T.length k % T.length n
    else 0 % 1
  where
    k = T.toLower keyword
    n = T.toLower name

-- | Prompt the user to choose among the given projects.
chooseMatch ::
  NonEmpty Project ->
  IO Project
chooseMatch (x :| _) = do
  print x
  return x

-- | Obtain absolute path to the project directory.
projectDir ::
  -- | Project root
  ProjectsRoot ->
  -- | Project of interest
  Project ->
  -- | Absolute path to the project
  IO (Path Abs Dir)
projectDir (ProjectsRoot projectRoot) Project {..} = do
  ownerDir <- parseRelDir (T.unpack projectOwner)
  nameDir <- parseRelDir (T.unpack projectName)
  return (projectRoot </> ownerDir </> nameDir)

----------------------------------------------------------------------------
-- Command line options parsing

data Opts = Opts
  { -- | Keyword that identifies the project.
    optKeyword :: Text
  }

optsParserInfo :: ParserInfo Opts
optsParserInfo =
  info (helper <*> ver <*> optsParser) . mconcat $
    [ fullDesc,
      progDesc "p"
    ]
  where
    ver :: Parser (a -> a)
    ver =
      infoOption verStr . mconcat $
        [ long "version",
          short 'v',
          help "Print version of the program"
        ]
    verStr =
      intercalate
        "\n"
        [ unwords
            [ "p",
              showVersion version,
              $gitBranch,
              $gitHash
            ]
        ]

optsParser :: Parser Opts
optsParser =
  Opts
    <$> (argument str . mconcat)
      [ metavar "KEYWORD",
        help "Keyword that is used to identify the name of the project"
      ]
