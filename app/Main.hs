{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Data.List (intercalate)
import Data.Text (Text)
import Data.Text.Metrics
import Data.Version (showVersion)
import Development.GitRev
import Options.Applicative
import Path
import Path.IO
import Paths_project_jumper (version)

main :: IO ()
main = do
  Opts {..} <- execParser optsParserInfo
  print optKeyword

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
