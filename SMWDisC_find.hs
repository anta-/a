{-# LANGUAGE ViewPatterns #-}

module Main where

import System.Environment (getArgs)
import Control.Applicative
import Control.Monad
import Control.Arrow
import Data.Function
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

infixl 0 `traceSeq`
traceSeq s x = trace s () `seq` x

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

createHonkeCode :: Map.IntMap String -> Map.IntMap String -> Int -> Int -> (([(Int, String)], Int, Int), Map.IntMap String)
createHonkeCode q s addr0 size = printf "*** createHonkeCode _ _ %06X %X" addr0 size `traceSeq`
    createHonkeCode' q addr0 size True addr0 0
    where
        createHonkeCode' q addr 0 b a1 a2 = if Map.member addr s
            then (([], a1, a2), q)
            else createHonkeCode' q (addr+1) 0 b a1 (a2+1)
        createHonkeCode' q addr size b a1 a2 = printf "**** createHonkeCode' _ %06X %X %s %X %X" addr size (show b) a1 a2 `traceSeq`
            if Map.member addr s
            then
                let label = printf "HIJACK_%06X_%06X" addr0 addr in
                let ((t, a1', a2'), q') = createHonkeCode' (Map.insert addr label q) (addr+1) (size-1) False a1 (a2+1) in
                (((addr, label ++ ":\t" ++ honkeCodeTrans q' addr (s Map.! addr)) : t, a1', a2'), q')
            else if b
                then createHonkeCode' q (addr-1) (size+1) True (a1-1) (a2+1)
                else createHonkeCode' q (addr+1) (size-1) False a1 (a2+1)

honkeCodeTrans q a x
    | isBranch t = branchMod q a t
    | i == 0x4C = printf "jml $%06X" (a`div`0x10000*0x10000 + t!!2*0x100 + t!!1) -- jmp
    | i == 0x60 = jumpToRts a -- rts
    | t == [0x22, 0xDF, 0x86, 0x00] = executePtr a
    | otherwise = ("db "++). concat. intersperse ", ". map (printf"$%02X")$ t
    where
        t@(i:_) = map readHex'. words. fromJust$ regex1 x "^[^\\s]*:\\s+([0-9A-F\\s]{3,12})\\s*[^\\s]*\\s?[^\\s]*.*$"

isBranch :: [Int] -> Bool
isBranch = flip elem [0x10, 0x30, 0x50, 0x70, 0x90, 0xB0, 0xD0, 0xF0]. head

branchMod :: Map.IntMap String -> Int -> [Int] -> String
branchMod q a [b,x] = 
    printf "db $%02X, $04 : jml %s" (negBranch b) (fromMaybe (printf"$%06X"t) (t `Map.lookup` q))
    where
        t = a + 2 + neg80 x

-- 
executePtr :: Int -> String
executePtr a =
    let b = a + 3 in
    printf "sta $00 : lda.b #$%02X : pha : db $F4 : dw $%02X%02X : lda $00 : jml $0086DF"
        (b `div` 0x10000) (b `div` 0x100 `mod` 0x100) (b `mod` 0x100)

jumpToRts :: Int -> String
jumpToRts a = printf "jml $%06X" (
    [ 0x0080E7::Int, 0x01800D, 0x028BB8, 0x03818A
    , 0x048430, 0x058339, 0x06801F, 0x078101
    , 0x08800A, 0x098002, 0x0A8293, 0x0B805E
    , 0x0C9F5B, 0x0DA53C, 0x0E8056, 0x0F868A]!!(a`div`0x10000))

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
    printf "%s:\n%s\tjml $%06x" label (unlines$ code) (hjAddr + hjSize)
    ) :: (String, String)

regex1 s r = let (_,_,_,t) = s =~ r :: (String,String,String,[String]) in
    listToMaybe t

uniqBy :: (a -> a -> Bool) -> [a] -> [a]
uniqBy f (x:y:zs)
    | f x y = uniqBy f (y:zs)
    | otherwise = x : uniqBy f (y:zs)
uniqBy _ [x] = [x]
uniqBy _ [] = []

numberingSMWDisC :: [String] -> [String]
numberingSMWDisC =
    map snd. uniqBy ((==)`on`fst). sortBy (compare`on`fst). -- SMWDisCには重複部分がある
    f'. f 0. filter (`regexb` "^[^\\s]*:\\s+[0-9A-F]{2}[0-9A-F\\s]{0,9}\\s*[^\\s]*\\s?[^\\s]*.*$")
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
    Map.fromList$ filterMap (\x->
        (\a-> (readHex' a, x)) <$> regex1 x "^[^\\s]*([0-9A-F]{6}):"
        ) f

regexb s r = let (_,t,_) = s =~ r :: (String,String,String) in
    not. null$ t

longAdressing :: String -> String -> String
longAdressing m s_ =
    let s = f s_ in
    let t = replace "\\.w" ".l" s in
    if s `regexb` "lda|sta|ora|and|eor|adc|cmp|sbc"
    then if s `regexb` ",y"
        then printf "stx !itizi_ram : php : tyx : plp : %s : php : ldx !itizi_ram : plp" (replace ",y" ",x"$ t)
        else t
    else if s `regexb` "sty" && s `regexb` ",x"
        then printf "sta !itizi_ram : php : tya : plp : %s : php : lda !itizi_ram : plp" (replace "sty" "sta" t)
        else printf "PHB : PHP : db $F4 : dw %s>>8 : PLB : PLB : PLP : %s : PHP : sep #$20 : sta !itizi_ram : PLA : sta !itizi_ram2 : PLB : lda !itizi_ram2 : PHA : lda !itizi_ram : PLP" m s
    where
        -- .Bはつかない
        f x =
            if x `regexb` "[^\\.][^wl]\\s"
                then replace "\\s" ".w " x
                else x

findAndCreateHijacking f s q aa mm =
    let storeCodes = map (readHex' *** (longAdressing mm. formatSMWDisCtoXkas. substAddr aa mm)) (findSMWDisC f aa) in
    let (xs, (s', q')) = h q s storeCodes in
    (unzip xs, (s', q'))
    where
        g a t (xs,p,q) = (map (\(b,c)-> if b == a then replace ":\t.*$" ":\t" c ++ t else c) xs, p, q)
        h q s [] = ([], (s, q))
        h q s ((a,c):xs) = printf "** findAndCreateHijacking@h _ _ ((%06X,_):_)" a `traceSeq`
            let (z, q') =
                            createHonkeCode q s a 4 in
            let (y, s', q'') = branchMods q' s z in
            let (zs, sq) = h q'' s' xs in
            (map (createHijack. g a c) y ++ zs, sq)

branchMods :: Map.IntMap String -> Map.IntMap String -> ([(Int, String)], Int, Int) -> ([([(Int, String)], Int, Int)], Map.IntMap String, Map.IntMap String)
branchMods q s (c, a, z) = -- (printf "branchMods %06X %X" a z) `traceSeq`
    (f q (deletes s a z)$ map (a+) [-0x80..0x7F+z])
    where
        f q s [] = ([(c, a, z)], s, q)
        f q s (a':aa) = case a' `Map.lookup` s of
            Nothing -> f q s aa
            Just x -> let bs@(~[b,y]) = instBytes x in
                if isBranch bs
                then if (let t = a'+2+neg80 y in a<t && t<a+z)
                    then --(printf "branch %06X -> %06X (%06X)" a' a (a'+2+neg80 y)) `traceSeq`
                        let ((c_, a_, z_), q') = createHonkeCode q s a' 4 in
                        let (zs, s', q'') = branchMods q' s$ (c_, a_, z_) in
                        let (zs', s'', q''') = f q'' (deletes s' a_ z_) aa in
                        (zs ++ zs', s'', q''')
                    else f q s aa
                else f q s aa
        deletes s a z = foldl (flip Map.delete) s [a..a+z-1]

instBytes :: String -> [Int]
instBytes x = map readHex'. words. fromJust$ x `regex1` "^[^\\s]*:\\s+([0-9A-F]{2}[0-9A-F\\s]{0,9})\\s*[^\\s]*\\s?[^\\s]*.*$"

readHex' = fst. head. readHex

spriteTables f = do
    let s = smwDisCMap f
    ts <- zip [(0::Int)..]. map words. filter (not. isPrefixOf ";"). lines <$> readFile "sprite_tables.txt"
    let (x, y) = join(***)unlines$ unzip$ fst$ h ts s Map.empty
    writeFile "a1.log" x
    writeFile "a2.log" y
    where
        h :: [(Int,[String])] -> Map.IntMap String -> Map.IntMap String -> ([(String, String)], (Map.IntMap String, Map.IntMap String))
        h ((i,xs):ts) s q =
            let (zs, (s', q')) = h' xs i s q in
            let (ws, (s'', q'')) = h ts s' q' in
            (zs : ws, (s'', q''))
        h [] s q = ([], (s, q))
        h' (x:xs) i s q =
            let (zs, (s', q')) = g i f x ("!st_"++map(\x->if x=='$'then '_'else toLower x) x) s q in
            let (ws, (s'', q'')) = h' xs i s' q' in
            (zs `app2` ws, (s'', q''))
            where app2 (x,y) (z,w) = (x++z, y++w)
        h' [] _ s q = (([], []), (s, q))
        g i f a m s q = printf "* spriteTables@g %X _ %s _ _" i a `traceSeq`
            let (z, (s', q')) = findAndCreateHijacking f s q a m in
            (first (header++)$ join(***)unlines$ z, (s, q))
            where
                header = printf "%s = $%X*!st_size+!st_start\n" (map toLower m) i :: String

writeFie2 (x,y) = writeFile "a1.log" x >> writeFile "a2.log" y

main = do
    ar <- getArgs
    when (length ar == 1 && ar!!0 == "--st") ((spriteTables =<< smwDisCtext) >> fail "end")
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
    then writeFie2$ (first (headerCode aa m'++)$ join(***)unlines$ fst$ findAndCreateHijacking f s (Map.empty) a m)
    else putStrLn ((headerCode aa m'++)$ createRelocate a m f)


