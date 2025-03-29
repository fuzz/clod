{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Clod.FileSystem.TransformationsSpec
-- Description : Tests for file transformation operations
-- Copyright   : (c) Fuzz Leonard, 2025
-- License     : MIT
-- Maintainer  : cyborg@bionicfuzz.com
-- Stability   : experimental
--
-- This module contains tests for the FileSystem.Transformations module.

module Clod.FileSystem.TransformationsSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import System.Directory
import System.FilePath
import System.IO.Temp (withSystemTempDirectory)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Control.Monad (forM_, void)
import Data.Either (isRight)

import Polysemy
import Polysemy.Error
import Polysemy.Reader

import Clod.Types
import Clod.Effects
import Clod.Capability
import Clod.FileSystem.Transformations

-- | Create a test file with specific content
createTestFile :: FilePath -> String -> IO ()
createTestFile path content = do
  createDirectoryIfMissing True (takeDirectory path)
  BS.writeFile path (BS.pack $ map fromIntegral $ fromEnum <$> content)

-- | Property: flattenPath should never contain directory separators after transformation
prop_flattenPathNoSeparators :: String -> Property
prop_flattenPathNoSeparators s =
  not (null s) ==> not ('/' `elem` flattenPath s || '\\' `elem` flattenPath s)

-- | Property: flattenPath preserves the file extension
prop_flattenPathPreservesExtension :: String -> String -> Property
prop_flattenPathPreservesExtension name ext =
  not (null name) && not (null ext) && not ('.' `elem` ext) ==>
    takeExtension (flattenPath (name ++ "." ++ ext)) == "." ++ ext

spec :: Spec
spec = do
  describe "flattenPath" $ do
    it "removes path separators" $ property prop_flattenPathNoSeparators
    
    it "preserves file extensions" $ property prop_flattenPathPreservesExtension
    
    it "handles complex paths" $ do
      flattenPath "path/to/some/file.txt" `shouldNotContain` "/"
      flattenPath "path\\to\\some\\file.txt" `shouldNotContain` "\\"
      
    it "handles paths with multiple extensions" $ do
      flattenPath "file.tar.gz" `shouldEndWith` ".tar.gz"
      
    it "handles paths with dots in directory names" $ do
      let result = flattenPath "some.dir/file.txt"
      result `shouldNotContain` "/"
      result `shouldEndWith` ".txt"
  
  describe "sanitizeFilename" $ do
    it "removes special characters" $ do
      sanitizeFilename "file:with?invalid*chars" `shouldBe` "filewithinvalidchars"
      
    it "preserves alphanumeric characters" $ do
      sanitizeFilename "abcXYZ123" `shouldBe` "abcXYZ123"
      
    it "preserves extensions" $ do
      sanitizeFilename "file.with.dots.txt" `shouldEndWith` ".txt"
      
    it "handles unicode characters" $ do
      sanitizeFilename "filename_with_é_and_ü.txt" `shouldNotContain` "é"
      sanitizeFilename "filename_with_é_and_ü.txt" `shouldNotContain` "ü"
      sanitizeFilename "filename_with_é_and_ü.txt" `shouldEndWith` ".txt"
      
    it "handles empty strings" $ do
      sanitizeFilename "" `shouldBe` "unnamed"
  
  describe "transformFileContent" $ do
    it "transforms file content correctly" $ do
      withSystemTempDirectory "clod-test" $ \tmpDir -> do
        -- Create a test file
        let srcFile = tmpDir </> "source.txt"
            destFile = tmpDir </> "destination.txt"
            content = "Line 1\nLine 2\nLine 3"
            
        createTestFile srcFile content
        
        -- Define a simple transformation
        let transform = T.unlines . map (T.append "TRANSFORMED: ") . T.lines . TE.decodeUtf8
        
        -- Set up capabilities
        let readCap = fileReadCap [tmpDir]
            writeCap = fileWriteCap [tmpDir]
        
        -- Run the test
        result <- runM . runError . runFileSystemIO $ do
          transformFileContent readCap writeCap transform srcFile destFile
          safeFileExists readCap destFile
        
        -- Verify results
        isRight result `shouldBe` True
        
        -- Check the content of the transformed file
        destContent <- BS.readFile destFile
        let destText = TE.decodeUtf8 destContent
        T.unpack destText `shouldBe` "TRANSFORMED: Line 1\nTRANSFORMED: Line 2\nTRANSFORMED: Line 3\n"
    
    it "respects capability restrictions" $ do
      withSystemTempDirectory "clod-test" $ \tmpDir -> do
        -- Create directories
        let srcDir = tmpDir </> "source"
            destDir = tmpDir </> "destination"
            restrictedDir = tmpDir </> "restricted"
            
        forM_ [srcDir, destDir, restrictedDir] $ createDirectoryIfMissing True
        
        -- Create a test file
        let srcFile = srcDir </> "file.txt"
            destFile = destDir </> "file.txt"
            restrictedFile = restrictedDir </> "file.txt"
            content = "Test content"
            
        createTestFile srcFile content
        
        -- Define a simple transformation
        let transform = T.append "TRANSFORMED: " . TE.decodeUtf8
        
        -- Set up capabilities that don't include restricted dir
        let readCap = fileReadCap [srcDir, destDir]
            writeCap = fileWriteCap [destDir]
        
        -- Run the test for valid transformation
        validResult <- runM . runError . runFileSystemIO $ do
          transformFileContent readCap writeCap transform srcFile destFile
          safeFileExists readCap destFile
        
        -- Run the test for transformation to restricted location
        restrictedResult <- runM . runError . runFileSystemIO $ do
          transformFileContent readCap writeCap transform srcFile restrictedFile
        
        -- Verify results
        isRight validResult `shouldBe` True
        case restrictedResult of
          Left (ConfigError _) -> return () -- Expected error
          Left err -> expectationFailure $ "Wrong error type: " ++ show err
          Right _ -> expectationFailure "Should have failed due to restricted destination"