module Main where

import System.Environment

import Lib ( readExpr, eval )

main :: IO ()
main = getArgs >>= print . eval . readExpr . head
