uselessDoBlock1 :: IO ()
uselessDoBlock1 = do
                    putStrLn "Hey"
                    getLine
                    putStrLn "Boom!"

uselessDoBlock2 :: IO ()
uselessDoBlock2 = putStrLn "Hey" >> 
                  getLine >> 
                  putStrLn "Boom!"

uselessDoBlock3 :: IO ()
uselessDoBlock3 = putStrLn "Hey!" >>= 
                    (\_ -> getLine >>= 
                        (\_ -> putStrLn "Boom!")) 