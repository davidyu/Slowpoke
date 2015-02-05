{-# LANGUAGE MultiParamTypeClasses, DataKinds, KindSignatures #-}

module Math.Matrix where

import GHC.TypeLits

data Matrix (rows::Nat) (cols::Nat) = Matrix [Float] deriving (Show)
type Mat33 = Matrix 3 3
type Mat44 = Matrix 4 4

makeMat :: Nat -> Nat -> [Float] -> Matrix 3 3
makeMat r c xs = undefined -- if length xs == r*c then 

-- identity :: n => Int -> Matrix n n
-- identity n = undefined
