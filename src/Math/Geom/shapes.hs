{-# LANGUAGE QuasiQuotes #-}

module Math.Geom.Shapes where

import Math.Vec
import Math.Geom.Primitives
import Record

data Shape = Sphere   [record| {center :: Point , radius :: Double}      |]
           | Triangle [record| {v1 :: Point , v2 :: Point , v3 :: Point} |]
             deriving (Show)
