{-|
Module: Strict.Pair.Infix
Description: infix operators for @Pair@
License: PublicDomain
Stability: experimental
Portability: GHC

This module provides the infix type synonym @:!:@ and re-exports the pattern @:!:@.
-}

{-# LANGUAGE PatternSynonyms
           , TypeOperators
   #-}

module Strict.Pair.Infix (type (:!:), pattern (:!:)) where

import Strict.Pair

type (:!:) = Pair
