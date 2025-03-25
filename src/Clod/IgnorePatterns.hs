{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Clod.IgnorePatterns
-- Description : Functions for handling ignore patterns (.gitignore, .clodignore)
-- Copyright   : (c) fuzz, 2025
-- License     : MIT
-- Maintainer  : fuzz@github.com
-- Stability   : experimental
--
-- This module provides functionality for parsing and matching .gitignore and
-- .clodignore patterns to determine which files should be excluded.

module Clod.IgnorePatterns
  ( -- * Pattern reading functions
    readClodIgnore
  , readGitIgnore
    -- * Pattern matching functions
  , matchesIgnorePattern
  , simpleGlobMatch
  ) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.List as L
import System.Directory (doesFileExist)
import System.FilePath (splitDirectories, takeExtension, takeFileName, (</>))

import Clod.Types (ClodM, IgnorePattern)

-- | Read and parse .clodignore file
readClodIgnore :: FilePath -> ClodM [IgnorePattern]
readClodIgnore projectPath = do
  let ignorePath = projectPath </> ".clodignore"
  exists <- liftIO $ doesFileExist ignorePath
  if exists
    then do
      content <- liftIO $ readFile ignorePath
      return $ filter isValidPattern $ lines content
    else return []
  where
    isValidPattern line = not (null line) && not ("#" `L.isPrefixOf` line)

-- | Read and parse .gitignore file
readGitIgnore :: FilePath -> ClodM [IgnorePattern]
readGitIgnore projectPath = do
  let gitIgnorePath = projectPath </> ".gitignore"
  exists <- liftIO $ doesFileExist gitIgnorePath
  if exists
    then do
      content <- liftIO $ readFile gitIgnorePath
      let lines' = lines content
      -- Process each line to handle standard git patterns
      let validPatterns = filter isValidPattern lines'
      return validPatterns
    else return []
  where
    isValidPattern line = not (null line) && not ("#" `L.isPrefixOf` line) && not ("!" `L.isPrefixOf` line)

-- | Check if a file matches any ignore pattern
matchesIgnorePattern :: [IgnorePattern] -> FilePath -> Bool
matchesIgnorePattern patterns filePath =
  any (matchPattern filePath) patterns
  where
    matchPattern :: FilePath -> String -> Bool
    matchPattern path pattern
      -- Skip empty patterns
      | null pattern = False
      
      -- Normalize the pattern to remove trailing slashes for consistency
      | "/" `L.isSuffixOf` pattern = matchPattern path (init pattern)
      
      -- Special handling for common directory patterns that need to match at any level
      | pattern == "node_modules" || pattern == "/node_modules" =
          -- Explicitly check for node_modules in path components
          let pathComponents = splitDirectories path
          in "node_modules" `elem` pathComponents
          
      -- Handle leading slash (anchored to root)
      | "/" `L.isPrefixOf` pattern =
          let patternWithoutSlash = drop 1 pattern
          in matchFromRoot patternWithoutSlash path
              
      -- File extension pattern: *.ext
      | "*." `L.isPrefixOf` pattern = 
          let ext = drop 2 pattern  -- Skip "*."
              fileExt = takeExtension path
              -- Get extension without the dot, safely
              extWithoutDot = if null fileExt then "" else drop 1 fileExt
          in extWithoutDot == ext  -- Check extension equality
              
      -- Directory pattern inside path (contains slash)
      | '/' `elem` pattern = 
          let 
            -- Split both pattern and path into components
            patternComponents = splitDirectories pattern
            pathComponents = splitDirectories path
            
            -- For multi-component patterns, check if they match a subsequence of path components
            multiComponentMatch = any (L.isPrefixOf patternComponents) (tails pathComponents)
            
            -- Also check if pattern matches path directly
            directMatch = pattern `L.isPrefixOf` path || ("/" ++ pattern) `L.isPrefixOf` ("/" ++ path)
          in directMatch || multiComponentMatch
              
      -- Simple filename or pattern with no slashes - could be a directory name or a file
      | otherwise = 
          let
            fileName = takeFileName path
            -- Split path into components for directory matching
            pathComponents = splitDirectories path
            
            -- Check for exact filename match
            exactMatch = pattern == fileName
            
            -- Check for directory name match anywhere in path
            dirMatch = pattern `elem` pathComponents
            
            -- For common directory patterns like 'dist', 'build' - match them anywhere in path
            commonDirPatterns = ["dist", "build", "node_modules", "tmp", "temp"]
            isCommonDirPattern = pattern `elem` commonDirPatterns
          in exactMatch || dirMatch || (isCommonDirPattern && any (== pattern) pathComponents)
    
    -- Get all tails of a list
    tails :: [a] -> [[a]]
    tails [] = [[]]
    tails xs@(_:xs') = xs : tails xs'
    
    -- Match a pattern that should start from the root
    matchFromRoot :: String -> FilePath -> Bool
    matchFromRoot pattern path =
      let 
        -- Split both pattern and path into components
        patternComponents = splitDirectories pattern
        pathComponents = splitDirectories path
      in
        -- Root patterns must match from the beginning of the path
        L.isPrefixOf patternComponents pathComponents || 
        -- Special case: if the root pattern is a directory name (like "/node_modules"),
        -- it should match that directory anywhere in the path to be compatible with git behavior
        (length patternComponents == 1 && head patternComponents `elem` pathComponents)

-- | Simple glob pattern matching
simpleGlobMatch :: String -> String -> Bool
simpleGlobMatch [] [] = True
simpleGlobMatch ('*':xs) [] = simpleGlobMatch xs []
simpleGlobMatch _ [] = False
simpleGlobMatch [] _ = False
simpleGlobMatch ('*':'.':ext) path
    -- Special case for file extensions
    | not (null ext) =
        let fileExt = takeExtension path
        in if null fileExt
            then False
            else drop 1 fileExt == ext
simpleGlobMatch ('*':'*':'/':ps) path =
    -- **/ can match zero or more directories
    let restPath = dropWhile (/= '/') path
    in  simpleGlobMatch ('*':'*':'/':ps) (drop 1 restPath) || 
        simpleGlobMatch ps path || 
        simpleGlobMatch ps (drop 1 restPath)
simpleGlobMatch ('*':'*':ps) (c:cs) =
    -- ** can match zero or more characters 
    simpleGlobMatch ps (c:cs) || simpleGlobMatch ('*':'*':ps) cs
simpleGlobMatch ('*':ps) (c:cs) =
    -- * can match zero or more characters except /
    if c == '/' 
       then simpleGlobMatch ('*':ps) cs
       else simpleGlobMatch ps (c:cs) || simpleGlobMatch ('*':ps) cs
simpleGlobMatch ('?':ps) (_:cs) = simpleGlobMatch ps cs  -- ? matches any single character
simpleGlobMatch (p:ps) (c:cs)
    | p == c    = simpleGlobMatch ps cs
    | otherwise = False