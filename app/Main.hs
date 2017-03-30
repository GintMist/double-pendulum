module Main where
import Lib
import System.Environment (getArgs)

main = getArgs >>= (anim . fmap readF)
    where
        readF :: String -> Float
        readF = read