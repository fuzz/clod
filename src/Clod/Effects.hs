{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}

-- |
-- Module      : Clod.Effects
-- Description : Effect types and utilities for Clod
-- Copyright   : (c) Fuzz Leonard, 2025
-- License     : MIT
-- Maintainer  : ink@fuzz.ink
-- Stability   : experimental
--
-- This module provides types and utilities for working with effects in Clod.
-- It uses a traditional monad transformer stack rather than an algebraic effects
-- system, prioritizing type inference and error reporting clarity.
--
-- The module re-exports the core functionality from Clod.Types to provide
-- a clean interface for working with the ClodM monad stack.

module Clod.Effects 
  ( -- * Core effect types
    ClodM
  , ClodError(..)
  , ClodConfig(..)
  
    -- * Running effects
  , runClodM
  
    -- * Monadic operations
  , throwError
  , catchError
  , liftIO
  
    -- * Reader operations
  , ask
  , asks
  , local
  ) where

import Clod.Types
  ( ClodM
  , ClodError(..)
  , ClodConfig(..)
  , runClodM
  , throwError
  , catchError
  , liftIO
  , ask
  , asks
  , local
  )