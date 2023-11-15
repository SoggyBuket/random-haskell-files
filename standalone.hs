-- // A main, standalone file
module Main where

-- // running "ghc --make -o wassup standalone.hs" will automatically import
-- /  this and add it to the compilation
import Hello

main = putStrLn Hello.hello
