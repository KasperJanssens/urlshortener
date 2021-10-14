{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Server.Server as Server

main :: IO ()
main = Server.start 8080 "myUrlDatabase"
