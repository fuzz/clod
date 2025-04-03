{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      : Clod.Capability
-- Description : Capability-based security for file operations
-- Copyright   : (c) Fuzz Leonard, 2025
-- License     : MIT
-- Maintainer  : ink@fuzz.ink
-- Stability   : experimental
--
-- This module implements capability-based security for file operations,
-- providing safe access to the filesystem with explicit permissions.
-- It enforces the principle of least privilege by requiring explicit
-- capabilities for reading from and writing to files.
--
-- This security model ensures that the application can only access specific directories
-- that have been explicitly granted access. The capabilities are represented by tokens
-- (FileReadCap and FileWriteCap) that must be passed to functions that interact with the
-- filesystem.
--
-- Core principles:
--
-- * Files can only be read if they're in a directory allowed by FileReadCap
-- * Files can only be written if they're in a directory allowed by FileWriteCap
-- * Capabilities cannot be forged - they must be obtained from authorized sources
-- * Path traversal attacks are prevented through careful path validation
--
-- Example usage:
--
-- > -- Create capabilities with restricted access
-- > readCap <- mkFileReadCap ["/path/to/project"]
-- > writeCap <- mkFileWriteCap ["/path/to/staging"]
-- > 
-- > -- Use capabilities for filesystem operations
-- > content <- safeReadFile readCap "/path/to/project/src/main.hs"
-- > safeWriteFile writeCap "/path/to/staging/src-main.hs" content

module Clod.Capability 
  ( -- * Capability types and functionality are now in Types.hs
  ) where