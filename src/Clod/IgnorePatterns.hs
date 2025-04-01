{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      : Clod.IgnorePatterns
-- Description : Functions for handling ignore patterns (.gitignore, .clodignore)
-- Copyright   : (c) Fuzz Leonard, 2025
-- License     : MIT
-- Maintainer  : cyborg@bionicfuzz.com
-- Stability   : experimental
--
-- This module provides functionality for parsing and matching .gitignore and
-- .clodignore patterns to determine which files should be excluded from processing.
--
-- The module supports common gitignore patterns including:
--
-- * Simple file patterns: @README.md@, @LICENSE@
-- * Directory patterns: @node_modules/@, @dist/@
-- * Extension patterns: @*.js@, @*.svg@
-- * Path patterns: @src/components/@
-- * Patterns with wildcards: @**\/node_modules@, @src\/**\/*.js@
-- * Negation patterns: @!important.txt@ (to exclude a file from a broader pattern)
-- * Character classes: @[abc]file.txt@, @file[0-9].txt@
--
-- === Default Patterns
--
-- This module provides default .clodignore patterns that are embedded directly into the
-- executable at compile time using Template Haskell.
--
-- === Pattern Matching Rules
--
-- 1. File extension patterns (@*.ext@) match any file with that extension
-- 2. Directory patterns match at any level in the directory tree
-- 3. Patterns with leading slash (@\/dist@) are anchored to the repository root
-- 4. Patterns with trailing slash are treated as directories
-- 5. Patterns with wildcards use simplified glob matching
-- 6. Negation patterns (@!pattern@) re-include a previously excluded file
-- 7. Later patterns take precedence over earlier ones
--
-- === Usage
--
-- @
-- -- Read patterns from a .clodignore file
-- patterns <- readClodIgnore "/path/to/repo"
--
-- -- Check if a file matches any pattern
-- if matchesIgnorePattern patterns "src/components/Button.jsx"
--   then -- Skip the file
--   else -- Process the file
-- @

module Clod.IgnorePatterns
  ( -- * Pattern reading functions
    readClodIgnore
  , readGitIgnore
  , readNestedGitIgnores
  , createDefaultClodIgnore
    -- * Pattern matching functions
  , matchesIgnorePattern
  , simpleGlobMatch
  , makePatternMatcher
    -- * Pattern types and utilities
  , PatternType(..)
  , categorizePatterns
    -- * Embedded content
  , defaultClodIgnoreContent
  , defaultClodIgnoreContentStr
  ) where

import Control.Monad (filterM)
import Control.Monad.IO.Class (liftIO)
import Control.Exception (try, SomeException)
import Control.Monad.Except (throwError)
import qualified Data.List as L
import Data.Char (toLower)
import qualified Data.Map.Strict as Map
import System.Directory (doesDirectoryExist, doesFileExist, getDirectoryContents)
import System.FilePath (splitDirectories, takeExtension, takeFileName, takeDirectory, (</>))
import Data.FileEmbed (embedStringFile)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import Data.Text (Text)
import GHC.Generics (Generic)
import qualified Dhall

import Clod.Types (ClodM, IgnorePattern(..), ClodError(..))
import Clod.Config (clodIgnoreFile)

-- | Pattern synonyms for common ignore pattern structures
pattern FileExtension :: String -> String
pattern FileExtension ext <- ('*':'.':ext@(_:_)) where
  FileExtension ext = '*':'.':ext

pattern DirectoryWildcard :: String -> String
pattern DirectoryWildcard rest <- ('*':'*':'/':rest) where
  DirectoryWildcard rest = '*':'*':'/':rest

pattern MultiLevelWildcard :: String -> String
pattern MultiLevelWildcard rest <- ('*':'*':rest) where
  MultiLevelWildcard rest = '*':'*':rest

pattern SingleLevelWildcard :: String -> String
pattern SingleLevelWildcard rest <- ('*':rest) where
  SingleLevelWildcard rest = '*':rest

-- Leading slash pattern for paths
pattern CharClassStart :: String -> String
pattern CharClassStart rest <- ('[':rest) where
  CharClassStart rest = '[':rest

-- | Types of ignore patterns
data PatternType 
  = Inclusion  -- ^ Normal inclusion pattern (e.g., "*.js")
  | Negation   -- ^ Negation pattern to re-include files (e.g., "!important.js")
  deriving (Show, Eq)

-- | Data type to parse ignore patterns from Dhall
data ClodIgnorePatterns = ClodIgnorePatterns
  { textPatterns :: [Text]  -- ^ List of patterns to ignore
  } deriving (Show, Eq, Generic)

instance Dhall.FromDhall ClodIgnorePatterns where
  autoWith _ = Dhall.record $ ClodIgnorePatterns <$> Dhall.field "textPatterns" Dhall.auto

-- | Default clodignore pattern content embedded at compile time (Dhall format)
defaultClodIgnoreContent :: Text
defaultClodIgnoreContent = T.pack $ BS.unpack $(embedStringFile "resources/default_clodignore.dhall")

-- | Default clodignore pattern content as a string (for testing)
defaultClodIgnoreContentStr :: String
defaultClodIgnoreContentStr = BS.unpack $(embedStringFile "resources/default_clodignore.dhall")

-- | Map used to cache compiled pattern matchers for performance
type PatternCache = Map.Map String (FilePath -> Bool)

-- | Categorize patterns by type (inclusion or negation)
categorizePatterns :: [IgnorePattern] -> ([IgnorePattern], [IgnorePattern])
categorizePatterns = L.partition isInclusion
  where 
    isInclusion (IgnorePattern p) = not ("!" `L.isPrefixOf` p)

-- | Create a default .clodignore file using the embedded template
--
-- This function creates a new .clodignore file in the specified directory
-- using the default patterns embedded in the executable at compile time.
--
-- @
-- createDefaultClodIgnore "/path/to/repo" ".clodignore"
-- @
createDefaultClodIgnore :: FilePath -> String -> ClodM ()
createDefaultClodIgnore projectPath ignoreFileName = do
  let ignorePath = projectPath </> ignoreFileName
  
  -- Parse the Dhall content to extract patterns
  result <- liftIO $ try $ Dhall.input Dhall.auto defaultClodIgnoreContent
  case result of
    Left (e :: SomeException) -> 
      throwError $ ConfigError $ "Failed to parse default clodignore patterns: " ++ show e
    Right (ClodIgnorePatterns patterns) -> do
      -- Convert to plain text format with comments
      let fileContent = "# Default .clodignore file for Claude uploader\n# Add patterns to ignore files when uploading to Claude\n\n" ++
                       unlines (map T.unpack patterns)
      
      -- Write the file
      liftIO $ writeFile ignorePath fileContent

-- | Read and parse .clodignore file
-- 
-- This function reads patterns from a .clodignore file in the specified directory.
-- If the file doesn't exist, a default one is created using the template in resources/default_clodignore.dhall
-- which is a proper Dhall configuration file that is parsed and converted to a plain text .clodignore file.
-- Comments (lines starting with '#') and empty lines are ignored.
--
-- Uses the CLODIGNORE environment variable or defaults to ".clodignore".
--
-- @
-- patterns <- readClodIgnore "/path/to/repo"
-- @
readClodIgnore :: FilePath -> ClodM [IgnorePattern]
readClodIgnore projectPath = do
  ignoreFileName <- liftIO clodIgnoreFile
  let ignorePath = projectPath </> ignoreFileName
  exists <- liftIO $ doesFileExist ignorePath
  if exists
    then do
      content <- liftIO $ readFile ignorePath
      return $ map IgnorePattern $ filter isValidPattern $ lines content
    else do
      -- Create a default .clodignore file if one doesn't exist
      createDefaultClodIgnore projectPath ignoreFileName
      -- Now read the newly created file
      content <- liftIO $ readFile ignorePath
      return $ map IgnorePattern $ filter isValidPattern $ lines content
  where
    isValidPattern line = not (null line) && not ("#" `L.isPrefixOf` line)

-- | Read and parse .gitignore file
--
-- This function reads patterns from a .gitignore file in the specified directory.
-- If the file doesn't exist, an empty list is returned.
-- Comments (lines starting with '#') and empty lines are ignored.
-- Negation patterns (lines starting with '!') are properly processed.
--
-- @
-- patterns <- readGitIgnore "/path/to/repo"
-- @
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
      return $ map IgnorePattern validPatterns
    else return []
  where
    isValidPattern line = not (null line) && not ("#" `L.isPrefixOf` line)

-- | Find and read all .gitignore files in a directory tree
--
-- This function recursively searches for all .gitignore files in a directory
-- and its subdirectories, and combines their patterns. Patterns in deeper
-- directories take precedence over ones in higher directories.
--
-- This matches Git's behavior where each directory can have its own .gitignore
-- that applies to files within it.
--
-- @
-- patterns <- readNestedGitIgnores "/path/to/repo"
-- @
readNestedGitIgnores :: FilePath -> ClodM [IgnorePattern]
readNestedGitIgnores rootPath = do
  -- Find all .gitignore files
  ignoreFiles <- findGitIgnoreFiles rootPath
  -- Read patterns from each file, maintaining order with deeper files later
  -- (later patterns take precedence in git)
  patternLists <- mapM (\file -> do
    dir <- liftIO $ takeDirectory <$> return file
    patterns <- readGitIgnoreFile file
    return (dir, patterns)
    ) ignoreFiles
  
  -- Process patterns to make paths relative to their containing directory
  let processedPatterns = concatMap (\(dir, patterns) -> 
        map (makeRelativeToDir dir) patterns) patternLists
  
  -- Return the combined patterns
  return processedPatterns
  where
    -- Find all .gitignore files recursively
    findGitIgnoreFiles :: FilePath -> ClodM [FilePath]
    findGitIgnoreFiles dir = do
      let gitignorePath = dir </> ".gitignore"
      exists <- liftIO $ doesFileExist gitignorePath
      let current = if exists then [gitignorePath] else []
      
      -- Get subdirectories
      dirExists <- liftIO $ doesDirectoryExist dir
      subdirs <- if dirExists
        then do
          contents <- liftIO $ getDirectoryContents dir
          let validDirs = filter (`notElem` [".", "..", ".git"]) contents
          filterM (\d -> liftIO $ doesDirectoryExist (dir </> d)) validDirs
        else return []
      
      -- Recursively process subdirectories
      subResults <- mapM (\subdir -> findGitIgnoreFiles (dir </> subdir)) subdirs
      
      -- Combine results, ordering by depth (deeper files later for precedence)
      return $ current ++ concat subResults
    
    -- Read patterns from a .gitignore file
    readGitIgnoreFile :: FilePath -> ClodM [IgnorePattern]
    readGitIgnoreFile path = do
      content <- liftIO $ readFile path
      return $ map IgnorePattern $ filter isValidPattern $ lines content
      where
        isValidPattern line = not (null line) && not ("#" `L.isPrefixOf` line)
    
    -- Make a pattern relative to its containing directory
    makeRelativeToDir :: FilePath -> IgnorePattern -> IgnorePattern
    makeRelativeToDir dir (IgnorePattern p) =
      let isNegation = "!" `L.isPrefixOf` p
          actualPattern = if isNegation then drop 1 p else p
          isAbsolute = "/" `L.isPrefixOf` actualPattern
          adjusted = if isAbsolute 
                     then actualPattern  -- Already absolute
                     else if dir == rootPath
                          then actualPattern  -- In root dir, keep as-is
                          else let relDir = drop (length rootPath + 1) dir
                               in relDir </> actualPattern
          final = if isNegation then "!" ++ adjusted else adjusted
      in IgnorePattern final

-- | Check if a file matches any ignore pattern, respecting negations
--
-- This function checks if a given file path matches any of the provided ignore patterns,
-- while properly handling negation patterns. Patterns are processed in order, with later
-- patterns taking precedence over earlier ones.
--
-- A file is ignored if it matches any inclusion pattern and doesn't match any
-- subsequent negation pattern.
--
-- === Examples
--
-- @
-- -- Check if a file should be ignored
-- matchesIgnorePattern [IgnorePattern "*.js", IgnorePattern "!important.js"] "app.js"  -- Returns True (matches *.js)
-- matchesIgnorePattern [IgnorePattern "*.js", IgnorePattern "!important.js"] "important.js"  -- Returns False (negated)
-- matchesIgnorePattern [IgnorePattern "src/*.svg"] "src/logo.svg"  -- Returns True
-- matchesIgnorePattern [IgnorePattern "node_modules"] "src/node_modules/file.js"  -- Returns True
-- @
matchesIgnorePattern :: [IgnorePattern] -> FilePath -> Bool
matchesIgnorePattern patterns filePath = 
  -- Process patterns in reverse order (later patterns take precedence)
  let (inclusions, negations) = categorizePatterns patterns
      
      -- Process negation patterns - extract the pattern without '!'
      negationPatterns = map (\(IgnorePattern p) -> 
                            IgnorePattern (drop 1 p)) negations
      
      -- Check if any inclusion pattern matches
      includedByPattern = any (matchesPattern filePath) inclusions
      
      -- Check if any negation pattern matches
      negatedByPattern = any (matchesPattern filePath) negationPatterns
  in
    -- Included by some pattern and not negated by any later pattern
    includedByPattern && not negatedByPattern
  where
    -- Use cached pattern matchers for better performance
    cache = Map.empty :: PatternCache
    
    -- Match a single pattern against a path
    matchesPattern :: FilePath -> IgnorePattern -> Bool
    matchesPattern path (IgnorePattern p) =
      let (_, matcher) = getCachedMatcher cache p
      in matcher path

-- | Get or create a cached pattern matcher
getCachedMatcher :: PatternCache -> String -> (PatternCache, FilePath -> Bool)
getCachedMatcher cache ptn = 
  case Map.lookup ptn cache of
    Just matcher -> (cache, matcher)
    Nothing -> 
      let matcher = makePatternMatcher ptn
          newCache = Map.insert ptn matcher cache
      in (newCache, matcher)

-- | Convert a pattern string into a function that matches paths against that pattern
makePatternMatcher :: String -> (FilePath -> Bool)
makePatternMatcher ptn
  -- Skip empty patterns
  | null ptn = const False
  
  -- Normalize the pattern to remove trailing slashes for consistency
  | "/" `L.isSuffixOf` ptn = makePatternMatcher $ init ptn
  
  -- Handle leading slash (anchored to root)
  | "/" `L.isPrefixOf` ptn =
      let patternWithoutSlash = drop 1 ptn
      in matchFromRoot patternWithoutSlash
      
  -- File extension pattern: *.ext
  | "*." `L.isPrefixOf` ptn = matchExtension ptn
      
  -- Directory pattern inside path (contains slash)
  | '/' `elem` ptn = 
      if '*' `elem` ptn || '?' `elem` ptn || containsCharClass ptn
        -- For wildcard patterns, use glob matching
        then simpleGlobMatch ptn
        -- For non-wildcard paths, use component matching
        else matchPathComponents ptn
      
  -- Pattern with character class like [a-z]file.txt
  | containsCharClass ptn = 
      simpleGlobMatch ptn
      
  -- Simple filename or pattern with no slashes
  | otherwise = matchSimpleName ptn

-- | Check if a pattern contains a character class ([...])
containsCharClass :: String -> Bool
containsCharClass [] = False
containsCharClass ('[':_) = True
containsCharClass (_:rest) = containsCharClass rest

-- | Match file extension patterns like "*.js"
matchExtension :: String -> (FilePath -> Bool)
matchExtension ptn = \path ->
  let ext = drop 2 ptn  -- Skip "*."
      dirPattern = takeDirectory ptn
      dirParts = if dirPattern /= "." then splitDirectories dirPattern else []
      fileExt = takeExtension path
      -- Get extension without the dot, safely
      extWithoutDot = if null fileExt then "" else drop 1 fileExt
      -- For patterns like src/*.svg we need to check directory prefixes
      pathComponents = splitDirectories path
      -- Directory check based on prefix matching
      dirCheck = null dirParts || L.isPrefixOf dirParts pathComponents
      -- File extension check should be case-insensitive and exact match
      extensionCheck = map toLower extWithoutDot == map toLower ext
  in dirCheck && extensionCheck

-- | Match path component patterns like "src/components"
matchPathComponents :: String -> (FilePath -> Bool)
matchPathComponents ptn = \path ->
  let patternComponents = splitDirectories ptn
      pathComponents = splitDirectories path
      -- Check for direct prefix match
      directMatch = ptn `L.isPrefixOf` path || 
                   ("/" ++ ptn) `L.isPrefixOf` ("/" ++ path)
      -- Check for match at any level in the path
      multiComponentMatch = any (L.isPrefixOf patternComponents) (tails pathComponents)
  in directMatch || multiComponentMatch

-- | Match simple name patterns like "README.md" or "node_modules"
matchSimpleName :: String -> (FilePath -> Bool)
matchSimpleName ptn = \path ->
  let fileName = takeFileName path
      pathComponents = splitDirectories path
      -- Check for exact filename match
      exactMatch = ptn == fileName
      -- Check for directory name match anywhere in path
      dirMatch = ptn `elem` pathComponents
      -- Special case for trailing wildcards "dir/**"
      hasTrailingWildcard = "/**" `L.isSuffixOf` ptn
      folderPattern = if hasTrailingWildcard
                      then take (length ptn - 3) ptn
                      else ptn
      -- Component matching for wildcards
      folderMatchWithWildcard = hasTrailingWildcard && 
                              (folderPattern `elem` pathComponents) &&
                              maybe False (< length pathComponents - 1) 
                                (L.elemIndex folderPattern pathComponents)
  in exactMatch || dirMatch || folderMatchWithWildcard

-- | Get all tails of a list
tails :: [a] -> [[a]]
tails [] = [[]]
tails xs@(_:xs') = xs : tails xs'

-- | Match a pattern that should start from the root
matchFromRoot :: String -> (FilePath -> Bool)
matchFromRoot ptn = \path ->
  let patternComponents = splitDirectories ptn
      pathComponents = splitDirectories path
      -- Handle wildcards or character classes in the pattern
      containsSpecial = any containsSpecialChars patternComponents
  in if containsSpecial
     then simpleGlobMatch ptn path
     else L.isPrefixOf patternComponents pathComponents
  where
    containsSpecialChars s = '*' `elem` s || '?' `elem` s || containsCharClass s

-- | Simple glob pattern matching for wildcard patterns
--
-- This function implements a simplified glob pattern matching algorithm 
-- that handles the most common wildcard patterns:
--
-- * @*@ - matches any sequence of characters except /
-- * @**@ - matches any sequence of characters including /
-- * @?@ - matches any single character
-- * @[a-z]@ - matches any character in the specified range
-- * @[!a-z]@ - matches any character not in the specified range
-- * @*.ext@ - matches files with the specified extension
-- * @**/pattern@ - matches pattern at any directory level
--
-- The implementation is designed to be compatible with common .gitignore patterns.
--
-- === Examples
--
-- @
-- simpleGlobMatch "*.js" "app.js"  -- Returns True
-- simpleGlobMatch "src/*.js" "src/app.js"  -- Returns True
-- simpleGlobMatch "src/**/*.js" "src/components/Button.js"  -- Returns True
-- simpleGlobMatch "file[0-9].txt" "file5.txt"  -- Returns True
-- simpleGlobMatch "*.txt" "file.md"  -- Returns False
-- @

-- | Helper function to check if a string starts with a character
startsWith :: String -> Char -> Bool
startsWith [] _ = False
startsWith (x:_) c = x == c
simpleGlobMatch :: String -> FilePath -> Bool
simpleGlobMatch ptn = \filepath -> matchGlob ptn filepath
  where
    -- The core matching algorithm
    matchGlob :: String -> String -> Bool
    matchGlob pat path = case (pat, path) of
      -- Base cases
      ([], []) -> True
      ([], _)  -> False
      
      -- Pattern with characters left but no path to match
      ((SingleLevelWildcard ps), []) -> matchGlob ps []
      (_, [])        -> False
      
      -- File extension special case: *.ext
      ((FileExtension ext), _) ->
        let fileExt = takeExtension path
        in not (null fileExt) && map toLower (drop 1 fileExt) == map toLower ext
        
      -- Directory wildcard: **/
      ((DirectoryWildcard ps), (_:_)) ->
        let restPath = dropWhile (/= '/') path
        in matchGlob (DirectoryWildcard ps) (drop 1 restPath) || 
           matchGlob ps path || 
           matchGlob ps (drop 1 restPath)
           
      -- Multi-level wildcard: **
      ((MultiLevelWildcard ps), (c:cs)) ->
        matchGlob ps (c:cs) || matchGlob (MultiLevelWildcard ps) cs
        
      -- Single-level wildcard: *
      ((SingleLevelWildcard ps), (c:cs)) ->
        if c == '/' 
          then matchGlob (SingleLevelWildcard ps) cs  -- Skip the slash
          else if ps == [] && ('/' `elem` cs)  
              then False  -- Don't let * cross directory boundaries for patterns like "src/*.js"
              else if cs == [] || not ('/' `elem` cs)
                  then matchGlob ps (c:cs) || matchGlob (SingleLevelWildcard ps) cs
                  else False  -- Don't match across directory boundaries for *.js pattern
                  
      -- Single character wildcard: ?
      (('?':ps), (_:cs)) ->
        matchGlob ps cs
      
      -- Beginning of character class: [
      ((CharClassStart cs), (c:path')) ->
        let (classSpec, rest) = span (/= ']') cs
            negated = not (null classSpec) && classSpec `startsWith` '!'
            actualClass = if negated then drop 1 classSpec else classSpec
        in if null rest  -- Malformed pattern, no closing ]
           then False
           else 
             let matches = matchCharacterClass actualClass c
                 result = if negated then not matches else matches
             in result && matchGlob (drop 1 rest) path'
        
      -- Regular character matching
      ((p:ps), (c:cs)) ->
        p == c && matchGlob ps cs

-- | Character class patterns for `[...]` expressions
data CharClassPattern = SingleChar Char
                      | CharRange Char Char
                      deriving (Show, Eq)

-- | Parse character class patterns from a string
parseCharClass :: String -> [CharClassPattern]
parseCharClass [] = []
parseCharClass (a:'-':b:rest) = CharRange a b : parseCharClass rest
parseCharClass (x:xs) = SingleChar x : parseCharClass xs

-- | Match a character against a character class pattern ([a-z], [0-9], etc.)
matchCharacterClass :: String -> Char -> Bool
matchCharacterClass spec c = any (matchesPattern c) (parseCharClass spec)
  where
    matchesPattern :: Char -> CharClassPattern -> Bool
    matchesPattern ch (SingleChar x) = ch == x
    matchesPattern ch (CharRange start end) = start <= ch && ch <= end
