{-# LANGUAGE OverloadedStrings, RecordWildCards, ViewPatterns #-}
module Main where

import System.Environment (getArgs)
import Control.Exception (assert)
import Control.Applicative
import Control.Monad
import Control.Monad.RWS
import Data.Monoid
import Control.Arrow
import Data.Function
import Data.List
import Data.Maybe
import Data.Char
import Text.Printf
import qualified Data.IntMap as Map
import qualified Data.Array as Array
import Numeric
import Control.DeepSeq
import Debug.Trace
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString as BB
import qualified Data.Attoparsec.Char8 as PS
import Disassemble

main = undefined

findRAMAccess :: Int -> ST [Address]
findRAMAccess = undefined

createLongAddressing :: Address -> ST ()
createLongAddressing a = do
    breakBytes a 4  -- size of JML
    addNewCode c
    where
        c = NewCode
            { ncOriginalAddress = a
            , ncLabel = BS.pack$ printf "LA_%06X" a
            , ncType = LongAddressing
            }

breakBytes :: Address -> Size -> ST ()
breakBytes a size = mapM_ breakOriginAddress =<< getAddressOrigins a size

breakOriginAddress :: Address -> ST ()
breakOriginAddress = modifyAddressState (const True)

getAddrssOrigin :: Address -> ST Address
getAddrssOrigin a = (Array.! a). addressOrigin <$> ask

getAddressOrigins :: Address -> Size -> ST [Address]
getAddressOrigins a size = unique <$> mapM getAddrssOrigin [a..a+size-1]

modifyAddressState :: (AddressState -> AddressState) -> Address -> ST ()
modifyAddressState f a = modifyAddressStateMap$ Map.adjust f a

getAddressStateMap :: ST AddressStateMap
getAddressStateMap = get

getAddressInfoMap :: ST AddressInfoMap
getAddressInfoMap = addressInfoMap <$> ask

getAddressInfo :: Address -> ST AddressInfo
getAddressInfo a = (Map.! a) <$> getAddressInfoMap

getAddressBroken :: Address -> ST Bool
getAddressBroken a = (Map.! a) <$> getAddressStateMap

modifyAddressStateMap :: (AddressStateMap -> AddressStateMap) -> ST ()
modifyAddressStateMap = modify

addNewCode :: NewCode -> ST ()
addNewCode c = tell [c]

initalReadState :: BS.ByteString -> Bytes -> ReadState
initalReadState s r = t
    where
        m = initalAddressInfoMap s r
        t = ReadState
            { addressOrigin = createAddressOrigin m
            , addressInfoMap = initalAddressInfoMap s r
            }

initalSt :: BS.ByteString -> Bytes -> (ReadState, AddressStateMap)
initalSt s r = (m, Map.map (const False)$ addressInfoMap m)
    where
        m = initalReadState s r

createAddressOrigin :: AddressInfoMap -> AddressOrigin
createAddressOrigin m = Array.array (0, fst (last ts)) ts
    where
        ts = concatMap f (Map.assocs m)
        f (a, AddressInfo { aiBytes = (BB.length -> l)}) =
            map (\a'-> (a', a)) [a..a+l-1]

initalAddressInfoMap :: BS.ByteString -> Bytes -> AddressInfoMap
initalAddressInfoMap s r = Map.fromList$ createAddressInfos s r

type Address = Int
type Size = Int

type AddressState = Bool

-- 新しいコードは、その他のhijack情報が必要なこともあるので、
-- 種類だけ覚えておいて、コードは最後に生成する
-- BrokenJumpはBrokenなAddressに対してを検索してそこにBrokenJump+Brokenに…をやる感じで？
data NewCode = NewCode
    { ncOriginalAddress :: Int
    , ncLabel :: BS.ByteString
    , ncType :: NewCodeType
    } deriving (Show)

data NewCodeType = LongAddressing | BrokenJump
    deriving (Show)

type AddressStateMap = Map.IntMap AddressState
type AddressInfoMap = Map.IntMap AddressInfo
type AddressOrigin = Array.Array Int Int

data ReadState = ReadState
    { addressOrigin :: AddressOrigin
    , addressInfoMap :: AddressInfoMap
    } deriving (Show)

type ST = RWS ReadState [NewCode] AddressStateMap

type Bytes = BB.ByteString

data AddressInfo = AddressInfo
    { aiBytes :: Bytes
    , aiAssembly :: Maybe Assembly  -- Nothingの場合、Dataである
    } deriving (Show)

createAddressInfos :: BS.ByteString -> Bytes -> [(Int, AddressInfo)]
createAddressInfos smwDisC rom =
    smwDisCAddressToAddressInfoWithROM
        (smwDisCAddressUnionData$ addressSMWDisC$ parseSMWDisCFile smwDisC)
        rom

smwDisCAddressUnionData :: [SMWDisCAddress] -> [SMWDisCAddress]
smwDisCAddressUnionData xs = f xs
    where
        f ((x@SMWDisCAddress {sdaIsData = True}):(y@SMWDisCAddress {sdaIsData = True}):zs) =
            f (SMWDisCAddress
                { sdaIsData = True
                , sdaAddress = sdaAddress x
                , sdaLength = sdaLength x + sdaLength y
                } : zs)
        f (x:xs) = x : f xs
        f x = x

smwDisCAddressToAddressInfoWithROM :: [SMWDisCAddress] -> Bytes -> [(Int, AddressInfo)]
smwDisCAddressToAddressInfoWithROM s x = f (BB.drop 0x200 x) s
    where
        f _ [] = []
        f x (SMWDisCAddress {..} : xs) = (sdaAddress, AddressInfo
            { aiBytes = t
            , aiAssembly = guard (not sdaIsData) >> disassembleCode t
            }) : f d xs
            where (t, d) = BB.splitAt sdaLength x

addressSMWDisC :: [SMWDisC] -> [SMWDisCAddress]
addressSMWDisC x = f 0 x
    where
        f n [] = []
        f n (SMWDisC {..}:xs) =
            SMWDisCAddress
                { sdaIsData = fromMaybe False sdcIsData
                , sdaAddress = a
                , sdaLength = sdcLength
                } : f (a+sdcLength) xs
            where
                a = case sdcAddress of
                    Just (snesAddressToEnum-> x) -> if n == x
                        then x
                        else error$ printf "sdcAddress: %X /= %X" n x
                    Nothing -> n

type SNESAddress = Int

data SMWDisC = SMWDisC
    { sdcIsData :: Maybe Bool
    , sdcAddress :: Maybe SNESAddress
    , sdcLength :: Int
    } deriving (Show)

data SMWDisCAddress = SMWDisCAddress
    { sdaIsData :: Bool
    , sdaAddress :: Int
    , sdaLength :: Int
    } deriving (Show)

snesAddressToEnum :: SNESAddress -> Int
snesAddressToEnum x = x `div` 0x10000 * 0x8000 + x `mod` 0x8000

smwDisCFile :: IO BS.ByteString
smwDisCFile = BS.readFile "SMWDisC.txt"

romFile :: IO BB.ByteString
romFile = BB.readFile "base.smc"

parseSMWDisCFile :: BS.ByteString -> [SMWDisC]
parseSMWDisCFile = parseSMWDisC. f 1. BS.lines
    where
        -- 間違っている(邪魔な)行を取り除いたり、加工する
        -- 0DEFFE+3からかなり抜けてるが、まあ何もしなくていいよね
        f n [] = []
        f n (x:xs)
            | n == 1852 || n == 1853
            || (13874 <= n && n <= 14469)
            || (22295 <= n && n <= 22319)
            || n == 23645
            || n == 106180
            = f (n+1) xs
            -- bytesを追加
            | n == 33938 = "Instr02A4E5:        9D 0B 17      STA.W $170B,x " : f (n+1) xs
            | n == 35687 = "ADDR_02B31C:        95 B6         STA RAM_SpriteSpeedX,X " : f (n+1) xs
            -- スペース！
            | n == 107250 = "CODE_0DAA0D: A9 00         LDA.B #$00 " : f (n+1) xs
            -- 抜けてる…
            | n == 112156 = "CODE_0DE000:  E8  INX " : f (n+1) xs
            | n == 113412 = BS.pack ("DATA_0DF001:  "++data_0DF001++
                "  db "++(intercalate ",". map('$':). words$ data_0DF001)) : f (n+1) xs
            | otherwise = x : f (n+1) xs
        -- 明らかにcodeな気がするけどめんどくさいのでデータ扱いで
        data_0DF001 = "A5 00 85 02 8A 49 01 AA 20 7D A9 C6 01 D0 CE 20 08 AA A9 6B D0 05 20 08 AA A9 6C 20 5B A9 C6 00 D0 F4 20 08 AA A9 6D 97 6B 60 A4 57 A5 59 29 0F 85 00 A5 59 4A 4A 4A 4A 85 01 20 B1 A6 A6 00 20 08 AA A9 0F 20 5B A9 CA 10 F5 4C 5B F0 A6 00 20 0D AA A9 EA 20 5B A9 CA 10 F5 20 BA A6 20 7D A9 C6 01 10 E9 60 A2 02 4C CE EC 59 A4 57 A5 59 29 0F 85 00 A5 59 4A 4A 4A 4A AA 20 08 AA BF 6B F0 0D 20 5B A9 C6 00 10 F2 60"

parseSMWDisC :: [BS.ByteString] -> [SMWDisC]
parseSMWDisC = filterMap (eitherToMaybe. PS.parseOnly parseSMWDisCLine)

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right x) = Just x

parseSMWDisCLine :: PS.Parser SMWDisC
parseSMWDisCLine = isData <|> isNotData <|> unknownAddress <|> lengthOnly
    where
        isData = do
            PS.string "DATA_"
            a <- PS.hexadecimal
            PS.char ':'
            spaces
            SMWDisC (Just True) (Just a) <$> (bytesLength <|> dbdw)
        isNotData = do
            s <- PS.takeWhile (/= ' ') -- 先読み
            guard$ BS.length s >= 7   -- 汚い…
            let sa = BS.init$ BS.drop (BS.length s - 7) s
            a <- maybe (fail "aaa") return$ readMaybe readHex$ BS.unpack sa
            spaces
            l <- bytesLength
            return (SMWDisC (Just False) (Just a) l)
        unknownAddress = do
            PS.many1 (PS.satisfy (\c-> c /= ':' && c /= ' '))
            PS.char ':'
            spaces
            SMWDisC Nothing Nothing <$> dbdw <|> (do
                l <- bytesLength
                spaces
                (dat <|> code) <*> return l)
            where
                dat = PS.char '.' >> return (SMWDisC (Just True) Nothing)
                code = PS.satisfy PS.isAlpha_ascii >> return (SMWDisC (Just False) Nothing)
        dbdw = do
            PS.char '.'
            db <|> dw
            where
                db = PS.string "db" >> dbdws
                dw = PS.string "dw" >> (2*) <$> dbdws
                dbdws = PS.space >>
                    length <$> PS.sepBy (PS.many1 (PS.satisfy (\c-> c /= ' ' && c /= ','))) (PS.char ',')
        lengthOnly = do
            spaces
            SMWDisC Nothing Nothing <$> (bytesLength <|> dbdw)
        isHexDigit c = ('A' <= c && c <= 'F') || ('0' <= c && c <= '9')
        bytes = PS.many1 (do
            x <- PS.hexadecimal :: PS.Parser Int
            PS.space
            return x)
        bytesLength = length <$> bytes
        spaces = PS.many1 PS.space

-- ユーティリティ
readMaybe :: ReadS a -> String -> Maybe a
readMaybe f x = case f x of
    [(y,"")] -> Just y
    _ -> Nothing

filterMap :: (a -> Maybe b) -> [a] -> [b]
filterMap f (x:xs) = case f x of
    Just y -> y : filterMap f xs
    Nothing -> filterMap f xs
filterMap _ [] = []

unique :: Eq a => [a] -> [a]
unique (x:y:zs)
    | x == y = unique (y:zs)
    | otherwise = x : unique (y:zs)
unique xs = xs

infixl 0 `traceSeq`
traceSeq s x = trace s () `seq` x

-- てすとこーど

io_SMWDisC :: IO [SMWDisC]
io_SMWDisC = parseSMWDisCFile <$> smwDisCFile

-- sdcLengthが妥当かどうか
addressCheck_parseSMWDisC :: [SMWDisC] -> [(SNESAddress, Bool)]
addressCheck_parseSMWDisC (x:SMWDisC {sdcAddress = Nothing, sdcLength = ly}:zs) =
    addressCheck_parseSMWDisC ((x {sdcLength = sdcLength x + ly}):zs)
addressCheck_parseSMWDisC (SMWDisC {sdcAddress=Just ax, sdcLength=lx}:y@SMWDisC {sdcAddress=Just ay}:zs) =
    (ax, ax+lx == ay) : addressCheck_parseSMWDisC (y:zs)
addressCheck_parseSMWDisC xs = []

test_addressCheck_parseSMWDisC = do
    s <- io_SMWDisC
    mapM_ (either print$ (>> putStrLn""). mapM_ print). map (g s. fst). filter (not. snd).
        addressCheck_parseSMWDisC. map f$ s
    where
        f x = x {sdcAddress = snesAddressToEnum <$> sdcAddress x}
        g s i = do
            j <- maybe (Left i) Right$ findIndex (\x-> Just i == sdcAddress (f x)) s
            return (take 10. drop (j-5). zip [0..]$ s)

io_AddressMap = do
    smwDisC <- smwDisCFile
    rom <- romFile
    return (createAddressInfos smwDisC rom)

io_initalReadState = do
    smwDisC <- smwDisCFile
    rom <- romFile
    return (initalReadState smwDisC rom)

io_initalSt = do
    smwDisC <- smwDisCFile
    rom <- romFile
    return (initalSt smwDisC rom)

assert' b = assert b `seq` return ()
