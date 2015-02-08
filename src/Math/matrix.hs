{-# LANGUAGE MultiParamTypeClasses, DataKinds, KindSignatures, TemplateHaskell #-}

module Math.Matrix where

import GHC.TypeLits
import Language.Haskell.TH

data Matrix (rows::Nat) (cols::Nat) = Matrix [Float] deriving (Show)
type Mat33 = Matrix 3 3
type Mat44 = Matrix 4 4

identity4 = Matrix [ 1, 0, 0, 0
                   , 0, 1, 0, 0
                   , 0, 0, 1, 0
                   , 0, 0, 0, 1 ] :: Mat44

identity3 = Matrix [ 1, 0, 0
                   , 0, 1, 0
                   , 0, 0, 1 ] :: Mat33
