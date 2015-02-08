{-# LANGUAGE MultiParamTypeClasses, DataKinds, KindSignatures #-}

module Math.Vec where

import GHC.TypeLits

data Vec (sz::Nat) = Vec [Double]
type Vec3 = Vec 3
type Vec4 = Vec 4
