{-# LANGUAGE MultiParamTypeClasses, DataKinds, KindSignatures, TemplateHaskell, FlexibleInstances #-}

module Math.Matrix where

import Math.Vec
import GHC.TypeLits
import Language.Haskell.TH

data Matrix (rows::Nat) (cols::Nat) = Matrix [Float] deriving (Show)

type Mat33 = Matrix 3 3
type Mat44 = Matrix 4 4

class MatAccessors (i::Nat) (j::Nat) where

class HomogeneousAccessors (n::Nat) where
  tx :: Matrix n n -> Float
  ty :: Matrix n n -> Float
  tz :: Matrix n n -> Float

identity4 = Matrix [ 1, 0, 0, 0
                   , 0, 1, 0, 0
                   , 0, 0, 1, 0
                   , 0, 0, 0, 1 ] :: Mat44

identity3 = Matrix [ 1, 0, 0
                   , 0, 1, 0
                   , 0, 0, 1 ] :: Mat33

instance HomogeneousAccessors 4 where
  tx (Matrix [ _, _, _, x
             , _, _, _, _
             , _, _, _, _
             , _, _, _, _ ] ) = x
  ty (Matrix [ _, _, _, _
             , _, _, _, y
             , _, _, _, _
             , _, _, _, _ ] ) = y
  tz (Matrix [ _, _, _, _
             , _, _, _, _
             , _, _, _, z
             , _, _, _, _ ] ) = z

instance HomogeneousAccessors 3 where
  tx (Matrix [ _, _, x
             , _, _, _
             , _, _, _ ] ) = x
  ty (Matrix [ _, _, _
             , _, _, y
             , _, _, _ ] ) = y
  tz  = error "tz undefined on 3x3 Matrix"
