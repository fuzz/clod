{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Clod.FileSystem.Transformations
-- Description : File type transformations for the Clod application
-- Copyright   : (c) Fuzz Leonard, 2025
-- License     : MIT
-- Maintainer  : cyborg@bionicfuzz.com
-- Stability   : experimental
--
-- This module handles special file type transformations required for Claude AI compatibility.
-- It contains transformations like SVG to XML conversion that are required due to 
-- external constraints from Claude's Project Knowledge system.
--
-- Note: These transformations exist solely due to limitations in Claude's file format support
-- and may be removed in future versions if those limitations are lifted.

module Clod.FileSystem.Transformations
  ( transformFilename
  ) where

import qualified Data.List as L

-- | Transform filename for Claude compatibility
--
-- This function applies special transformations to filenames to ensure 
-- they can be properly uploaded to Claude's Project Knowledge system.
--
-- Currently handles:
-- * SVG files - converted to XML extension for Claude compatibility
--
-- @
-- transformFilename "logo.svg" -- returns "logo-svg.xml"
-- transformFilename "image.png" -- returns "image.png" (no change)
-- @
transformFilename :: String -> String -> String
transformFilename name original
  -- SVG files must be transformed to XML for Claude compatibility
  | ".svg" `L.isSuffixOf` original = 
      -- For SVG files, convert to XML extension
      let baseName = take (length name - 4) name
      in baseName ++ "-svg.xml"
  -- All other files remain unchanged
  | otherwise = name
