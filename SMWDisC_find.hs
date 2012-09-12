{-# LANGUAGE ViewPatterns #-}

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
import qualified Data.IntMap.Lazy as Map
import Numeric

findSMWDisC :: [String] -> String -> [(String, String)]
findSMWDisC ls addr = filterMap f ls
    where
        f s = (\[a, i]-> (a, i)). tail <$> (listToMaybe$ s =~ r)
        r = "^[^\\s]*([0-9A-F]{6}):\\s+[0-9A-F\\s]{3,12}\\s*([^\\s]*\\s"++encode addr++"[^\\s]*).*$"

encode :: String -> String
encode = foldr (\x xs-> f x ++ xs) []
    where
        f '$' = "\\$"
        f '!' = "\\!"
        f c = [c]

filterMap :: (a -> Maybe b) -> [a] -> [b]
filterMap f (x:xs) = case f x of
    Just y -> y : filterMap f xs
    Nothing -> filterMap f xs
filterMap _ [] = []

formatSMWDisCtoXkas :: String -> String
formatSMWDisCtoXkas = map toLower

replace s u t =
    let (t1, t3, t2) = t =~ s :: (String,String,String) in
    if null t3 then t else t1 ++ u ++ t2

substAddr :: String -> String -> String -> String
substAddr a m t = replace (encode a) m t

orgCode :: (String, String) -> String
orgCode (a, s) = printf "org $%s\n\t%s" a s

headerCode :: String -> String -> String
headerCode a m = printf ";%s\n; \n%s = %s\n" (replicate 30 '=') m a

createRelocate a m f = (headerCode a m++). unlines.
        map (orgCode. second (formatSMWDisCtoXkas. substAddr a m))$
        findSMWDisC f a

createHonkeCode :: Map.IntMap String -> Int -> Int -> ([(Int, String)], Int, Int)
createHonkeCode s addr size = createHonkeCode' addr size True addr 0
    where
        createHonkeCode' addr 0 b a1 a2 = if Map.member addr s
            then ([], a1, a2)
            else createHonkeCode' (addr+1) 0 b a1 (a2+1)
        createHonkeCode' addr size b a1 a2 =
            if Map.member addr s
            then let (t, a1', a2') = createHonkeCode' (addr+1) (size-1) False a1 (a2+1) in
                ((addr, f (s Map.! addr)) : t, a1', a2')
            else if b
                then createHonkeCode' (addr-1) (size+1) True (a1-1) (a2+1)
                else createHonkeCode' (addr+1) (size-1) False a1 (a2+1)
        f s = (("db "++). concat. intersperse ", ". map ("$"++). words. fromJust) (regex1 s "^[^\\s]*:\\s+([0-9A-F\\s]{3,12})\\s*[^\\s]*\\s?[^\\s]*.*$")

createHijack (code, hjAddr, hjSize) =
    let label = printf "HIJACK_%06X" hjAddr :: String in
    (
    printf "org $%06X\n\tjml %s" hjAddr label,
    printf "%s:\n%s\tjml $%06x" label (unlines$ map ("\t"++) code) (hjAddr + hjSize)
    ) :: (String, String)

regex1 s r = let (_,_,_,t) = s =~ r :: (String,String,String,[String]) in
    listToMaybe t

numberingSMWDisC :: [String] -> [String]
numberingSMWDisC = map snd. f'. f 0. filter (`regexb` "^[^\\s]*:\\s+[0-9A-F]{2}[0-9A-F\\s]{0,9}\\s*[^\\s]*\\s?[^\\s]*.*$")
    where
        f' [] = []
        f' (x:xs) = case x of
            Left x ->
                let zs@((a',_):_) = f' xs in
                let n = length. words. fromJust$ x `regex1` "^[^\\s]*:\\s+([0-9A-F]{2}[0-9A-F\\s]{0,9})\\s*[^\\s]*\\s?[^\\s]*.*$" in
                let a = a' - n in
                (a, replace "^[^\\s]*:" (h a++":") x) : zs
            Right (a, x) -> (a, x) : f' xs
        f _ [] = []
        f n (x:xs) =
            let ns = words$ fromJust$ x `regex1` "^[^\\s]*:\\s+([0-9A-F]{2}[0-9A-F\\s]{0,9})\\s*[^\\s]*\\s?[^\\s]*.*$" in
            let (y, n') = g x in
            y : f ((fromMaybe n n')+length ns) xs
        g x = case x `regex1` "^[^\\s]*([0-9A-F]{6}):" of
            Nothing -> (Left x, Nothing)
            Just a' -> let a'' = h'. fst. head$ readHex a' in (Right (a'', x), Just (a''))
        h n = printf "%02X%04X" (n `div` 0x8000) (n `mod` 0x8000 + 0x8000)
        h' (subtract 0x8000-> n) = (n `div` 0x10000 * 0x8000) + (n `mod` 0x8000)

smwDisCtext = numberingSMWDisC <$> lines <$> readFile "SMWDisC.txt"
smwDisCMap f =
    Map.fromList$ filterMap (\x->
        (\a-> (fst. head$ readHex a, x)) <$> regex1 x "^[^\\s]*([0-9A-F]{6}):"
        ) f

regexb s r = let (_,t,_) = s =~ r :: (String,String,String) in
    not. null$ t

longAdressing :: String -> String -> String
longAdressing m (replace "\\.b" ".w" -> s) =
    if s `regexb` "lda|sta|ora|and|eor|adc|cmp|sbc"
    then if s `regexb` ",y"
        then printf "PHX : TYX : %s : PLX" (replace ",y" ",x"$ replace "\\.w" ".l" s)
        else replace "\\.w" ".l" s
    else printf "PHB : db $F4 : dw %s>>8 : PLB : PLB : %s : PLB" m s

findAndCreateHijacking f s aa mm = do
    let storeCodes = map (fst. head. readHex *** (longAdressing mm. formatSMWDisCtoXkas. substAddr aa mm)) (findSMWDisC f aa)
    unzip$ map (\(a, c)-> createHijack (g a c$ createHonkeCode s a 4)) storeCodes
    where
        g a t (xs,p,q) = (map (\(b,c)-> if b == a then t else c) xs, p, q)

spriteTables = do
    f <- smwDisCtext
    ts <- zip [(0::Int)..]. map words. filter (not. isPrefixOf ";"). lines <$> readFile "sprite_tables.txt"
    return$ unlines$ map (\(i, xs)-> concatMap (\x-> g i f x ("!st_"++map(\x->if x=='$'then '_'else x)x))xs) ts
    where
        g i f a m = (header++). unlines.
            map (orgCode. second (formatSMWDisCtoXkas. substAddr a m))$
            findSMWDisC f a
            where
                header = printf "%s = $%04X\n" (map toLower m) (0x0FBE+i*0xC) :: String

main = do
    ar <- getArgs
    when (length ar == 1 && ar!!0 == "--st") ((writeFile "st.asm" =<< spriteTables) >> fail "end")
    when (length ar < 2) (putStrLn "SMWDisC_find [find string] [variable name] [-h hijack(long addressing)]\nexample: SMWDisC_find $0FBE !pointer_of_16x" >> fail "end")
    let a = map toLower$ ar!!0
    let m = map toLower$ ar!!1
    f <- smwDisCtext
    let s = smwDisCMap f
    if length ar >= 3 && ar!!2 == "-h"
    then putStrLn ((headerCode a m++)$ (\(x,y)-> unlines x ++ "\n\n\n;"++replicate 30 '='++"\n; \n" ++ unlines y)$ findAndCreateHijacking f s a m)
    else putStrLn (createRelocate a m f)


