module Pseudolang.Main
    ( defaultMain
    ) where

import Pseudolang.Prelude

import Options.Applicative (Parser, ParserInfo, argument, execParser, fullDesc, header, help, helper, info, metavar, progDesc, str)

import Pseudolang.Interpreter (parseAndInterpret)

data CmdOpts = CmdOpts
  { file :: FilePath
  }

cmdOptsParser :: Parser CmdOpts
cmdOptsParser = do
  let fileParser =
        argument
          str
          ( metavar "FILE" <>
            help "Pseudolang file to interpret"
          )
  CmdOpts <$> fileParser

runOptionParser :: IO CmdOpts
runOptionParser = execParser opts
  where
    opts :: ParserInfo CmdOpts
    opts =
      info
        (cmdOptsParser <**> helper)
        ( fullDesc <>
          progDesc "Interpret and run FILE" <>
          header "pseudo - an interpreter for Pseudolang programs"
        )

defaultMain :: IO ()
defaultMain = do
  cmdOpts <- runOptionParser
  run cmdOpts

run :: CmdOpts -> IO ()
run (CmdOpts file) = do
  fileContents <- readFile file
  parseAndInterpret $ decodeUtf8 fileContents
