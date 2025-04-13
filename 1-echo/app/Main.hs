{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Node qualified

main :: IO ()
main = do
    _n <- Node.spawnNode nodeHandler
    pure ()

nodeHandler :: Int -> String
nodeHandler = show
