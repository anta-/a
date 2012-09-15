{-# LANGUAGE OverloadedStrings #-}

import System.Environment (getArgs)
import Control.Applicative
import Control.Monad
import Control.Arrow
import Data.Function
import Data.List
import Data.Maybe
import Data.Char
import Text.Printf
import qualified Data.IntMap as Map
import Numeric
import Control.DeepSeq
import Debug.Trace
import qualified Data.ByteString.Char8 as BS
import qualified Data.Attoparsec.Char8 as PS

type Bytes = BS.ByteString

type SNESAddress = Int

data AddressInfo = AddressInfo
    { aiIsData :: Bool
    , aiBytes :: Bytes
    } deriving (Show)
type AddressMap = Map.IntMap AddressInfo

data SMWDisC = SMWDisC
    { sdcIsData :: Maybe Bool
    , sdcAddress :: Maybe SNESAddress
    , sdcLength :: Int
    } deriving (Show)

snesAddressToEnum :: SNESAddress -> Int
snesAddressToEnum x = x `div` 0x10000 * 0x8000 + x `mod` 0x8000

createAddressMapWithSMWDisC :: [BS.ByteString] -> Bytes -> AddressMap
createAddressMapWithSMWDisC smwDisC rom =
    undefined

test_parseSMWDisC :: IO [SMWDisC]
test_parseSMWDisC = parseSMWDisCFile <$> BS.readFile "SMWDisC.txt"

-- sdcLengthが妥当かどうか
addressCheck_parseSMWDisC :: [SMWDisC] -> [(SNESAddress, Bool)]
addressCheck_parseSMWDisC (x:SMWDisC {sdcAddress = Nothing, sdcLength = ly}:zs) =
    addressCheck_parseSMWDisC ((x {sdcLength = sdcLength x + ly}):zs)
addressCheck_parseSMWDisC (SMWDisC {sdcAddress=Just ax, sdcLength=lx}:y@SMWDisC {sdcAddress=Just ay}:zs) =
    (ax, ax+lx == ay) : addressCheck_parseSMWDisC (y:zs)
addressCheck_parseSMWDisC xs = []

test_addressCheck_parseSMWDisC = do
    s <- test_parseSMWDisC
    mapM_ (either print $(>>putStrLn""). mapM_ print). map (g s). map fst. filter (not. snd).
        addressCheck_parseSMWDisC. map f$ s
    where
        f x = x {sdcAddress = snesAddressToEnum <$> sdcAddress x}
        g s i = do
            j <- maybe (Left i) Right$ findIndex (\x-> Just i == sdcAddress (f x)) s
            return (take 10. drop (j-5). zip [0..]$ s)

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
            | otherwise = x : f (n+1) xs

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
            s <- PS.takeWhile (\c-> c /= ' ') -- 先読み
            guard$ BS.length s >= 7   -- 汚い…
            let sa = BS.init$ BS.drop (BS.length s - 7) s
            a <- maybe (fail "aaa") return$ readMaybe readHex$ BS.unpack sa
            spaces
            l <- bytesLength
            return (SMWDisC (Just False) (Just a) l)
        unknownAddress = do
            PS.many1 (PS.satisfy (\c-> not (c == ':') && not (c == ' ')))
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

readMaybe :: ReadS a -> (String -> Maybe a)
readMaybe f x = case f x of
    [(y,"")] -> Just y
    _ -> Nothing

filterMap :: (a -> Maybe b) -> [a] -> [b]
filterMap f (x:xs) = case f x of
    Just y -> y : filterMap f xs
    Nothing -> filterMap f xs
filterMap _ [] = []

infixl 0 `traceSeq`
traceSeq s x = trace s () `seq` x

