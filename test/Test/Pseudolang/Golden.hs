module Test.Pseudolang.Golden where

import Pseudolang.Prelude

import System.Directory (listDirectory)
import System.FilePath (addExtension, isExtensionOf, takeFileName)
import Test.Hspec (Spec, describe, it, runIO, shouldBe)

import Pseudolang.Interpreter.Test (InterpResult(InterpResult), parseAndInterpretToResult)

data ExampleFile = ExampleFile
  { exampleFileRelPath :: FilePath
  , exampleFileContents :: ByteString
  , exampleFileExpectedOutput :: ByteString
  }
  deriving stock Show

data ExampleTree = Example ExampleFile | Directory FilePath [ExampleTree]
  deriving stock Show

buildExampleFile :: FilePath -> IO (Maybe ExampleFile)
buildExampleFile fp
  | ".pseudo" `isExtensionOf` fp = do
    exampleContents <- readFile fp
    expectedOutput <- readFile (addExtension fp ".expected-output")
    pure $ Just $ ExampleFile fp exampleContents expectedOutput
  | otherwise = pure Nothing

buildExampleTree' :: FilePath -> IO (Maybe ExampleTree)
buildExampleTree' fp = do
  eitherFiles <- try $ listDirectory fp
  case eitherFiles of
    -- This FilePath was a directory, and it contains files.  Build an
    -- ExampleTree out of its files.
    Right rawFilesOrDirectories -> do
      -- The files we get back from listDirectory are just file names, not full
      -- paths.  We have to append the path we've found so far before passing them
      -- on.
      let filesOrDirs = fmap (\fs -> fp </> fs) (sort rawFilesOrDirectories)
      exampleTrees <- traverse buildExampleTree' filesOrDirs
      pure $ Just $ Directory fp $ catMaybes exampleTrees
    -- IOException when trying to read the FilePath as a directory.
    -- This is probably a file.
    Left (_ :: IOException) -> do
      maybeExampleFile <- buildExampleFile fp
      pure $ fmap Example maybeExampleFile

buildExampleTree :: IO [ExampleTree]
buildExampleTree = do
  rawFilesOrDirectories <- listDirectory "examples"
  -- The files we get back from listDirectory are just file names, not full
  -- paths.  We have to append the path we've found so far before passing them
  -- on.
  let filesOrDirs = fmap (\fs -> "examples" </> fs) (sort rawFilesOrDirectories)
  exampleTrees <- traverse buildExampleTree' filesOrDirs
  pure $ catMaybes exampleTrees

outputTest :: Text -> Text -> IO ()
outputTest inputProgram expectedOutput = do
  InterpResult _ resOutput <- parseAndInterpretToResult inputProgram
  resOutput `shouldBe` expectedOutput

exampleTreeToSpec :: ExampleTree -> Spec
exampleTreeToSpec (Directory fp childTrees) = do
  describe (takeFileName fp) $ do
    traverse_ exampleTreeToSpec childTrees
exampleTreeToSpec (Example (ExampleFile relPath contents expectedOutput)) = do
  it (takeFileName relPath) $ do
    outputTest (decodeUtf8 contents) (decodeUtf8 expectedOutput)

test :: Spec
test =
  describe "Golden Tests" $ do
    exampleTrees <- runIO buildExampleTree
    traverse_ exampleTreeToSpec exampleTrees
