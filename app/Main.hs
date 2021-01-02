module Main where

import Lib ( readExpr )

main :: IO ()
main = do
    input <- getLine
    print $ readExpr input