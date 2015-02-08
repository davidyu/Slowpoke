{-# LANGUAGE MultiParamTypeClasses, DataKinds, KindSignatures #-}

module Math.Ops where

import GHC.TypeLits
import Math.Matrix

class MatMatOps (i::Nat) (j::Nat) (k::Nat) where
  mul :: Matrix i j -> Matrix j k -> Matrix i k
