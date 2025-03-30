{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Clod.FileSystem.TransformationsSpec
-- Description : Tests for the FileSystem.Transformations module
-- Copyright   : (c) Fuzz Leonard, 2025
-- License     : MIT
-- Maintainer  : cyborg@bionicfuzz.com
-- Stability   : experimental
--
-- This module tests the file transformations functionality.

module Clod.FileSystem.TransformationsSpec (spec) where

import Test.Hspec
import Test.QuickCheck ()
import System.Directory (doesFileExist, createDirectory)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Control.Exception ()
import Control.Monad ()
import Control.Monad.Except ()
import Control.Monad.Reader ()
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Either ()
import Data.Function ()
import Data.List ()

import Clod.Types
import Clod.FileSystem.Transformations
import Clod.TestHelpers (defaultTestConfig)

-- | Test the file transformations
spec :: Spec
spec = do
  describe "transformFilename" $ do
    it "transforms SVG files to XML with special suffix" $ do
      transformFilename "logo.svg" "logo.svg" `shouldBe` "logo-svg.xml"
      
    it "leaves non-SVG files unchanged" $ do
      transformFilename "main.js" "main.js" `shouldBe` "main.js"
      transformFilename "index.html" "index.html" `shouldBe` "index.html"
      
    it "transforms hidden files by removing dot and adding 'dot--' prefix" $ do
      transformFilename ".gitignore" ".gitignore" `shouldBe` "dot--gitignore"
      transformFilename ".tool-versions" ".tool-versions" `shouldBe` "dot--tool-versions"
      transformFilename ".config" ".config" `shouldBe` "dot--config"
      
    it "sanitizes filenames with special characters" $ do
      transformFilename "file with spaces.txt" "file with spaces.txt" `shouldBe` "filewithtxt"
      transformFilename "#weird$chars%.js" "#weird$chars%.js" `shouldBe` "weirdchars.js"
      transformFilename "$$$.svg" "$$$.svg" `shouldBe` "-svg.xml"
      
      
    it "returns a default name for empty filenames" $ do
      transformFilename "" "" `shouldBe` "unnamed"
      
  describe "flattenPath" $ do
    it "replaces directory separators with underscores" $ do
      flattenPath "dir/subdir/file.txt" `shouldBe` "dir_subdir_file.txt"
      flattenPath "some\\windows\\path.txt" `shouldBe` "some_windows_path.txt"
      
    it "leaves filenames without separators unchanged" $ do
      flattenPath "file.txt" `shouldBe` "file.txt"
      
  describe "sanitizeFilename" $ do
    it "removes invalid characters from filenames" $ do
      sanitizeFilename "hello world.txt" `shouldBe` "helloworld.txt"
      sanitizeFilename "test!@#$%^&*().txt" `shouldBe` "test.txt"
      sanitizeFilename "*special*.json" `shouldBe` "special.json"
      
    it "preserves valid characters" $ do
      sanitizeFilename "valid-name_123.js" `shouldBe` "valid-name_123.js"
      sanitizeFilename "a.b.c.d.e.f" `shouldBe` "a.b.c.d.e.f"
      
    it "returns 'unnamed' for empty strings" $ do
      sanitizeFilename "" `shouldBe` "unnamed"
      sanitizeFilename "#@$%^" `shouldBe` "unnamed"

  describe "transformFileContent" $ do
    it "transforms file content using the provided function" $ do
      -- Create a temp directory
      withSystemTempDirectory "clod-test" $ \tmpDir -> do
        -- Create a test file
        let srcPath = tmpDir </> "source.txt"
            destPath = tmpDir </> "dest.txt"
            content = "hello world"
            transformFn = T.toUpper . TE.decodeUtf8
            
        BS.writeFile srcPath (TE.encodeUtf8 (T.pack content))
        
        -- Create capabilities
        let readCap = fileReadCap [tmpDir]
            writeCap = fileWriteCap [tmpDir]
            config = defaultTestConfig tmpDir
        
        -- Run the function
        result <- runClodM config $ 
          transformFileContent readCap writeCap transformFn srcPath destPath
        
        -- Verify the destination file has the transformed content
        result `shouldBe` Right ()
        destContent <- TE.decodeUtf8 <$> BS.readFile destPath
        destContent `shouldBe` "HELLO WORLD"
        
    it "fails when source is outside read capability" $ do
      -- Create a temp directory structure
      withSystemTempDirectory "clod-test" $ \tmpDir -> do
        withSystemTempDirectory "clod-test-outside" $ \outsideDir -> do
          -- Create source file in the outside directory
          let srcPath = outsideDir </> "source.txt"
              destPath = tmpDir </> "dest.txt"
              content = "hello world"
              transformFn = T.toUpper . TE.decodeUtf8
              
          BS.writeFile srcPath (TE.encodeUtf8 (T.pack content))
          
          -- Create read capability that only includes the temp dir
          let readCap = fileReadCap [tmpDir]
              writeCap = fileWriteCap [tmpDir]
              config = defaultTestConfig tmpDir
          
          -- Run the function
          result <- runClodM config $ 
            transformFileContent readCap writeCap transformFn srcPath destPath
          
          -- Verify the operation failed
          case result of
            Left (CapabilityError _) -> return ()
            _ -> expectationFailure "Expected CapabilityError but got different result"
            
          -- Verify the destination file wasn't created
          destExists <- doesFileExist destPath
          destExists `shouldBe` False
          
  describe "SVG to XML transformation" $ do
    it "preserves SVG content with XML extension" $ do
      -- Create a temp directory structure
      withSystemTempDirectory "clod-test" $ \tmpDir -> do
        -- Create source and destination directories
        let srcDir = tmpDir </> "src"
            destDir = tmpDir </> "dest"
            svgPath = srcDir </> "icon.svg"
            xmlPath = destDir </> "icon-svg.xml"
            svgContent = "<svg xmlns='http://www.w3.org/2000/svg'><circle cx='50' cy='50' r='40'/></svg>"
            
        createDirectory srcDir
        createDirectory destDir
        BS.writeFile svgPath (TE.encodeUtf8 (T.pack svgContent))
        
        -- Create capabilities
        let readCap = fileReadCap [srcDir, destDir]
            writeCap = fileWriteCap [destDir]
            transformFn = TE.decodeUtf8 -- Transform ByteString to Text
            config = defaultTestConfig tmpDir
            
        -- Run the transformation
        _ <- runClodM config $ 
          transformFileContent readCap writeCap transformFn svgPath xmlPath
          
        -- Verify the XML file was created with the same content
        xmlContent <- TE.decodeUtf8 <$> BS.readFile xmlPath
        T.unpack xmlContent `shouldBe` svgContent

  describe "End-to-end transformation" $ do
    it "works with complex transformations" $ do
      -- Create a temp directory
      withSystemTempDirectory "clod-test" $ \tmpDir -> do
        -- Create test files
        let jsPath = tmpDir </> "script.js"
            htmlPath = tmpDir </> "page.html"
            jsContent = "console.log('Hello, world!');"
            htmlContent = "<html><body><h1>Test</h1></body></html>"
            
        BS.writeFile jsPath (TE.encodeUtf8 (T.pack jsContent))
        BS.writeFile htmlPath (TE.encodeUtf8 (T.pack htmlContent))
        
        -- Create capabilities
        let readCap = fileReadCap [tmpDir]
            writeCap = fileWriteCap [tmpDir]
            config = defaultTestConfig tmpDir
            
        -- Define a simple transformation function that adds different comments for different file types
        let jsTransform :: BS.ByteString -> T.Text
            jsTransform bs = "// Transformed by Clod\n" <> TE.decodeUtf8 bs
            
            htmlTransform :: BS.ByteString -> T.Text
            htmlTransform bs = "<!-- Transformed by Clod -->\n" <> TE.decodeUtf8 bs
              
        -- Run transformations
        let jsDest = tmpDir </> "script.transformed.js"
            htmlDest = tmpDir </> "page.transformed.html"
            
        _ <- runClodM config $ transformFileContent readCap writeCap jsTransform jsPath jsDest
        _ <- runClodM config $ transformFileContent readCap writeCap htmlTransform htmlPath htmlDest
        
        -- Verify results
        jsResult <- TE.decodeUtf8 <$> BS.readFile jsDest
        htmlResult <- TE.decodeUtf8 <$> BS.readFile htmlDest
        
        jsResult `shouldBe` "// Transformed by Clod\n" <> T.pack jsContent
        htmlResult `shouldBe` "<!-- Transformed by Clod -->\n" <> T.pack htmlContent

