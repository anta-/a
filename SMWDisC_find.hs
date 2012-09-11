module Main where

import System.Environment (getArgs)
import Control.Applicative
import Control.Monad
import Control.Arrow
import Data.List
import Data.Maybe
import Text.Regex.PCRE
import Data.Char
import Text.Printf

findSMWDisC :: [String] -> String -> [(String, String)]
findSMWDisC ls addr = filterMap f ls
    where
        f s = (\[a, i]-> (a, i)). tail <$> (listToMaybe$ s =~ r)
        r = "^[^\\s]*([0-9A-F]{6}):\\s+[0-9A-F\\s]{3,12}\\s*([^\\s]*[^#]"++encode addr++"[^\\s]*).*$"

encode :: String -> String
encode = foldr (\x xs-> f x ++ xs) []
    where
        f '$' = "\\$"
        f '!' = "\\!$"
        f c = [c]

filterMap :: (a -> Maybe b) -> [a] -> [b]
filterMap f (x:xs) = case f x of
    Just y -> y : filterMap f xs
    Nothing -> filterMap f xs
filterMap _ [] = []

formatSMWDisCtoXkas :: String -> String
formatSMWDisCtoXkas = map toLower

substAddr :: String -> String -> String -> String
substAddr a m t =
    let (t1, t3, t2) = t =~ encode a :: (String,String,String) in
    t1 ++ m ++ t2

orgCode :: (String, String) -> String
orgCode (a, s) = printf "org $%s\n\t%s" a s

headerCode :: String -> String -> String
headerCode a m = printf ";%s\n; \n%s = %s\n" (replicate 30 '=') m a

main = do
    ar <- getArgs
    when (length ar /= 2) (putStrLn "SMWDisC_find [find string] [variable name]\nexample: SMWDisC_find $0FBE !pointer_of_16x" >> fail "end")
    let a = ar!!0
    let m = ar!!1
    f <- lines <$> readFile "SMWDisC.txt"
    putStrLn ((headerCode a m++). unlines.
        map (orgCode. second (formatSMWDisCtoXkas. substAddr a m))$
        findSMWDisC f a)
