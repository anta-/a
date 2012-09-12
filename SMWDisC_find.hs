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
import qualified Data.IntMap as Map
import qualified Data.IntSet as Set
import Numeric
import Control.DeepSeq
import Debug.Trace

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

createRelocate a m f = unlines.
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
                ((addr, honkeCodeTrans addr (s Map.! addr)) : t, a1', a2')
            else if b
                then createHonkeCode' (addr-1) (size+1) True (a1-1) (a2+1)
                else createHonkeCode' (addr+1) (size-1) False a1 (a2+1)

honkeCodeTrans a x
    | isBranch t = branchMod a t
    | i == 0x4C = printf "jml $%02X%02X%02X" (a`div`0x10000) (t!!2) (t!!1) -- jmp
    | i == 0x60 = jumpToRts a -- rts
    | t == [0x22, 0xDF, 0x86, 0x00] = executePtr a
    | otherwise = ("db "++). concat. intersperse ", ". map (printf"$%02X")$ t
    where
        t@(i:_) = map readHex'. words. fromJust$ regex1 x "^[^\\s]*:\\s+([0-9A-F\\s]{3,12})\\s*[^\\s]*\\s?[^\\s]*.*$"

isBranch :: [Int] -> Bool
isBranch = flip elem [0x10, 0x30, 0x50, 0x70, 0x90, 0xB0, 0xD0, 0xF0]. head

branchMod :: Int -> [Int] -> String
branchMod a [b,x] = printf "db $%02X, $04 : jml $%06X" (negBranch b) (a + 2 + neg80 x)

-- JSL ExecutePtr があって、安全なところにJMLしちゃう
executePtr :: Int -> String
executePtr a = printf "jml $%06X" (
    [ 0x009325::Int, 0x01BDE6, 0x028B94, 0x039434
    , 0x04DAF4, 0x05BCEC, undefined, undefined
    , undefined, undefined, undefined, undefined
    , 0x0CC9D2, undefined, undefined, undefined]!!(a`div`0x10000))

negBranch :: Int -> Int
negBranch 0x10 = 0x30
negBranch 0x30 = 0x10
negBranch 0x50 = 0x70
negBranch 0x70 = 0x50
negBranch 0x90 = 0xB0
negBranch 0xB0 = 0x90
negBranch 0xD0 = 0xF0
negBranch 0xF0 = 0xD0

neg80 x
    | x >= 0x80 = x - 0x100
    | otherwise = x

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
                let n = length (instBytes x) in
                let a = a' - n in
                (a, replace "^[^\\s]*:" (h a++":") x) : zs
            Right (a, x) -> (a, x) : f' xs
        f _ [] = []
        f n (x:xs) =
            let ns = instBytes x in
            let (y, n') = g x in
            y : f ((fromMaybe n n')+length ns) xs
        g x = case x `regex1` "^[^\\s]*([0-9A-F]{6}):" of
            Nothing -> (Left x, Nothing)
            Just a' -> let a'' = h'. readHex'$ a' in (Right (a'', x), Just (a''))
        h n = printf "%02X%04X" (n `div` 0x8000) (n `mod` 0x8000 + 0x8000)
        h' (subtract 0x8000-> n) = (n `div` 0x10000 * 0x8000) + (n `mod` 0x8000)

smwDisCtext = (numberingSMWDisC <$> lines <$> )$ (return$!!) =<< readFile "SMWDisC.txt"
smwDisCMap f =
    (`Map.difference` excludeAddresses). Map.fromList$ filterMap (\x->
        (\a-> (readHex' a, x)) <$> regex1 x "^[^\\s]*([0-9A-F]{6}):"
        ) f

regexb s r = let (_,t,_) = s =~ r :: (String,String,String) in
    not. null$ t

longAdressing :: String -> String -> String
longAdressing m (replace "\\.b" ".w" -> s) =
    if s `regexb` "lda|sta|ora|and|eor|adc|cmp|sbc"
    then if s `regexb` ",y"
        then printf "stx !itizi_ram : TYX : %s : ldx !itizi_ram" (replace ",y" ",x"$ replace "\\.w" ".l" s)
        else replace "\\.w" ".l" s
    else printf "PHB : db $F4 : dw %s>>8 : PLB : PLB : %s : PLB" m s

findAndCreateHijacking f s aa mm = do
    let storeCodes = map (readHex' *** (longAdressing mm. formatSMWDisCtoXkas. substAddr aa mm)) (findSMWDisC f aa)
    unzip$ h s storeCodes
    where
        g a t (xs,p,q) = (map (\(b,c)-> if b == a then t else c) xs, p, q)
        h _ [] = []
        h s ((a,c):xs) = let (y, s') = branchMods s$ createHonkeCode s a 4 in
            map (createHijack. g a c) y ++ h s' xs

branchMods :: Map.IntMap String -> ([(Int, String)], Int, Int) -> ([([(Int, String)], Int, Int)], Map.IntMap String)
branchMods s (c, a, z) = --trace (printf "branchMods %06X %X" a z) () `seq`
    (f (deletes s a z)$ map (a+) [-0x80..0x7F+z])
    where
        f s [] = ([(c, a, z)], s)
        f s (a':aa) = case a' `Map.lookup` s of
            Nothing -> f s aa
            Just x -> let bs@(~[b,y]) = instBytes x in
                if isBranch bs
                then if (let t = a+2+neg80 y in a<t && t<a+z)
                    then
                        let (c_, a_, z_) = createHonkeCode s a' 4 in
                        let (zs, s') = branchMods s$ (c_, a_, z_) in
                        let (zs', s'') = f (deletes s' a_ z_) aa in
                        (zs ++ zs', s'')
                    else f s aa
                else f s aa
        deletes s a z = foldl (flip Map.delete) s [a..a+z-1]

instBytes :: String -> [Int]
instBytes x = map readHex'. words. fromJust$ x `regex1` "^[^\\s]*:\\s+([0-9A-F]{2}[0-9A-F\\s]{0,9})\\s*[^\\s]*\\s?[^\\s]*.*$"

readHex' = fst. head. readHex

spriteTables = do
    f <- smwDisCtext
    ts <- zip [(0::Int)..]. map words. filter (not. isPrefixOf ";"). lines <$> readFile "sprite_tables.txt"
    return$ unlines$ map (\(i, xs)-> concatMap (\x-> g i f x ("!st_"++map(\x->if x=='$'then '_'else x)x))xs) ts
    where
        g i f a m = (header++). unlines.
            map (orgCode. second (formatSMWDisCtoXkas. substAddr a m))$
            findSMWDisC f a
            where
                header = printf "%s = $%X*!st_size+!st_start\n" (map toLower m) i :: String

writeFie2 (x,y) = writeFile "a1.log" x >> writeFile "a2.log" y

-- 特殊なもののため、自分で書くために、考えないもの
excludeAddresses = Map.fromList. flip zip (repeat undefined)$
    []

main = do
    ar <- getArgs
    when (length ar == 1 && ar!!0 == "--st") ((writeFile "st.asm" =<< spriteTables) >> fail "end")
    when (length ar < 2) (putStrLn "SMWDisC_find [find string[=$addr]] [variable name] [-h hijack(long addressing)]\nexample: SMWDisC_find $0FBE !pointer_of_16x" >> fail "end")
    let a_ = ar!!0
    let (a, aa) =
                    case findIndex (=='=') a_ of
                        Nothing -> (a_, a_)
                        Just i -> let (xs,(_:ys)) = splitAt i a_ in (xs, ys)
    let m = ar!!1
    let m' = map toLower m
    f <- smwDisCtext
    let s = smwDisCMap f
    if length ar >= 3 && ar!!2 == "-h"
    then writeFie2$ (first (headerCode aa m'++)$ join(***)unlines$ findAndCreateHijacking f s a m)
    else putStrLn ((headerCode aa m'++)$ createRelocate a m f)


