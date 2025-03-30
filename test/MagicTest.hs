{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Magic.Init as Magic
import qualified Magic.Operations as Magic
import qualified Magic.Types as Magic
import Control.Exception (try, SomeException)
import System.IO (hPutStrLn, stderr)
import System.Directory (doesFileExist)

import Data.List (isPrefixOf, isInfixOf)

-- | Helper to determine if a MIME type represents text content
isMimeTypeText :: String -> Bool
isMimeTypeText mime =
  "text/" `isPrefixOf` mime ||
  mime == "application/json" ||
  mime == "application/xml" ||
  mime == "application/javascript" ||
  mime == "application/x-shell" ||
  mime == "application/x-shellscript" ||
  "script" `isInfixOf` mime

-- | Check if a file is a text file using libmagic
isTextFile :: FilePath -> IO Bool
isTextFile file = do
  exists <- doesFileExist file
  if not exists
    then return False
    else do
      result <- try $ do
        magic <- Magic.magicOpen [Magic.MagicMimeType]
        Magic.magicLoadDefault magic
        mime <- Magic.magicFile magic file
        return $ isMimeTypeText mime
      case result of
        Left (e :: SomeException) -> do
          hPutStrLn stderr $ "Error detecting file type: " ++ show e
          return False
        Right isText -> return isText

main :: IO ()
main = do
  let files = [
        "/Users/fuzz/Projects/clod-contain/CLAUDE.md",
        "/Users/fuzz/Projects/clod-contain/clod/README.md",
        "/Users/fuzz/Projects/clod-contain/clod/bin/cld",
        "/Users/fuzz/Projects/clod-contain/clod/src/Clod/Types.hs"
        ]
  
  putStrLn "Testing magic-based file detection:"
  mapM_ testFile files
  where
    testFile path = do
      result <- isTextFile path
      let resultStr = if result then "TEXT" else "BINARY"
      putStrLn $ path ++ ": " ++ resultStr