{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

module Disassemble
    ( Code, Assembly (..), Operand (..)
    , Operand', Assembly', PatchCode (..)
    , disassembleCode
    , isMemoryAccessAddressing, getOperandInt
    , getJumpAddress
    , assemblyToAssembly'
    , assemblyToLongAddressing, assemblyToLongJump, genSpecialCode, genRTSCode
    , showAssembly', showPatchCode
    , assembly'Length
    , isExecutePtrLong
    , assemblyToAACO
    , createGetDrawInfoPLA
    ) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString as BB
import Data.Bits
import qualified Data.Array as Array
import Control.Monad
import Control.Applicative
import Data.Maybe
import Data.Word (Word8)
import Text.Printf (printf)

createGetDrawInfoPLA :: Int -> [Assembly']
createGetDrawInfoPLA a =
    [ d [0xF0, 5+4]
    ] ++ replicate 5 (d [0x68])
    ++ next
    where
        next
            | bank == 1 = [ d [0x5C, 0xCD, 0xA3, 0x01]]    -- JML。ここが壊れてるとだめだけど…問題ないよね
            | bank == 2 || bank == 3 = genRTSCode' bank
            | otherwise = error$ "createGetDrawInfoPLA: bank == " ++ show bank
        bank = a `div` 0x10000
        d = assemblyToAssembly'. fromJust. disassembleCode. BB.pack

assemblyToAACO :: Assembly -> BS.ByteString -> Maybe [PatchCode]
assemblyToAACO (Assembly {..}) macro = do
    guard$ isAbsAddressingMode addressingMode
    return [ CodeAssembly (Assembly' mnemonic addressingMode (Opr'Macro macro)) ]
    where
        isAbsAddressingMode (StaticSAM a) = case a of
            AM_ABS -> True
            AM_ABSX -> True
            AM_ABSY -> True
            AM_ABSI -> True
            AM_ABSXI -> True
            AM_ABSIL -> True
            _ -> False
        isAbsAddressingMode _ = False

isExecutePtrLong :: Assembly -> Maybe Bool
isExecutePtrLong (Assembly
    { mnemonic = NM_JSL, operand = OprLong 0x86DF }) = Just False
isExecutePtrLong (Assembly
    { mnemonic = NM_JSL, operand = OprLong 0x86FA }) = Just True
isExecutePtrLong _ = Nothing

genRTSCode' :: Int -> [Assembly']
genRTSCode' bank =
    [ Assembly' NM_JML (StaticSAM AM_LONG) (Opr'Opr$ OprLong rtsAddr)
    ]
    where
        rtsAddr = bankRTSlist!!bank
        bankRTSlist =
            [ 0x0080E7, 0x01800D, 0x028BB8, 0x03818A
            , 0x048430, 0x058339, 0x06801F, 0x078101
            , 0x08800A, 0x098002, 0x0A8293, 0x0B805E
            , 0x0C9F5B, 0x0DA53C, 0x0E8056, 0x0F868A
            ]

genRTSCode :: Int -> Assembly -> Maybe [Assembly']
genRTSCode bank (Assembly {..})
    | mnemonic == NM_RTS = Just (genRTSCode' bank)
    | otherwise = Nothing

genSpecialCode :: Int -> Assembly -> Maybe [Assembly']
genSpecialCode a asm@(Assembly {..})
    | isJust e = executePtr (fromJust e)
    | otherwise = Nothing
    where
        e = isExecutePtrLong asm
        executePtr isLong =
            let b = a + 3 in
            Just
                [ d [0x85, 0x00]    -- STA $00
                , d [0xA9, b `div` 0x10000] -- LDA #$
                , d [0x48]  -- PHA
                , d [0xF4, b `mod` 0x100, b `div` 0x100 `mod` 0x100]    -- PEA
                , d [0xA5, 0x00]    -- LDA $00
                , d [0x5C, if isLong then 0xFA else 0xDF, 0x86, 0x00]    -- JML $0086DF / 0086FA
                ]
        d = assemblyToAssembly'. fromJust. disassembleCode. BB.pack. map fromIntegral

assembly'Length :: Assembly' -> Int
assembly'Length (Assembly' {..}) =
    1 + getSizedAddressingModeSize addressingMode'

showPatchCode :: PatchCode -> BS.ByteString
showPatchCode (CodeDB b) = BS.concat$
    "db " : tail (concatMap (\x-> [",", "$", BB.pack (showHex2 x)])$ BB.unpack b)
showPatchCode (CodeAssembly a) = showAssembly' a
showPatchCode (CodeRaw s _) = s

showHex2 :: Word8 -> [Word8]
showHex2 x = let (q, r) = quotRem x 16 in
    [ hexDigit q, hexDigit r ]
    where
        hexDigit d
            | d >= 0xA = d + (65 - 0xA)
            | otherwise = d + 48

showAssembly' :: Assembly' -> BS.ByteString
showAssembly' (Assembly' {..}) =
    if BS.null opr
    then mnem
    else BS.concat [mnem, " ", opr]
    where
        mnem = showMnemonic' mnemonic' addressingMode'
        opr = case operand' of
            Opr'Opr o ->
                let (o1, o2, o3) = showOperand o addressingMode' in
                BS.concat [o1, o2, o3]
            Opr'Macro m ->
                let (o1, _, o3) = showOperand undefined addressingMode' in
                BS.concat [o1, m, o3]

showMnemonic' :: Mnemonic -> SizedAddressingMode -> BS.ByteString
showMnemonic' m x = showMnemonic m `BS.append` size
    where
        b = ".b"
        w = ".w"
        l = ".l"
        size = case x of
            StaticSAM a' -> case a' of
                AM_DIR -> b
                AM_IMM8 -> b
                AM_PCR -> b
                AM_DIRS -> b
                AM_DIRX -> b
                AM_DIRY -> b
                AM_ABS -> w
                AM_PCRL -> w
                AM_ABSX -> w
                AM_ABSY -> w
                AM_LONG -> l
                AM_LONGX -> l
                AM_DIRI -> b
                AM_DIRIY -> b
                AM_DIRSIY -> b
                AM_DIRXI -> b
                AM_ABSI -> w
                AM_ABSXI -> w
                AM_ABSIL -> w
                AM_DIRIL -> b
                AM_DIRILY -> b
                _ -> ""
            SizedSAM a' -> case a' of
                SAM_IMM8 -> b
                SAM_IMM16 -> w


showMnemonic :: Mnemonic -> BS.ByteString
showMnemonic m = mnemonicStrArray Array.! m

showOperand :: Operand -> SizedAddressingMode -> (BS.ByteString, BS.ByteString, BS.ByteString)
showOperand o a = case a of
    StaticSAM a' -> case a' of
        AM_NONE -> ("","","")
        AM_DIR -> ("", p "$%02X", "")
        AM_IMM8 -> ("#", p "$%02X", "")
        AM_PCR -> ("", p "$%02X", "")
        AM_RAW -> (BS.pack (printf "$%02X, $%02X" a1 a2), "", "")
            where (a1, a2) = case o of Opr2Byte x y -> (x, y); _ -> error$ "showOperand: "++show o
        AM_DIRS -> ("", p "$%02X", ",s")
        AM_DIRX -> ("", p "$%02X", ",x")
        AM_DIRY -> ("", p "$%02X", ",y")
        AM_ABS -> ("", p "$%04X", "")
        AM_PCRL -> ("", p "$%04X", "")
        AM_ABSX -> ("", p "$%04X", ",x")
        AM_ABSY -> ("", p "$%04X", ",y")
        AM_LONG -> ("", p "$%06X", "")
        AM_LONGX -> ("", p "$%06X", ",x")
        AM_DIRI -> ("(", p "$%02X", ")")
        AM_DIRIY -> ("(", p "$%02X", "),y")
        AM_DIRSIY -> ("(", p "$%02X", ",s),y")
        AM_DIRXI -> ("(", p "$%02X", ",x)")
        AM_ABSI -> ("(", p "$%04X", ")")
        AM_ABSXI -> ("(", p "$%04X", ",x)")
        AM_ABSIL -> ("[", p "$%04X", "]")
        AM_DIRIL -> ("[", p "$%02X", "]")
        AM_DIRILY -> ("[", p "$%02X", "],y")
        AM_ACC -> ("A", "", "")
    SizedSAM a' -> case a' of
        SAM_IMM8 -> ("#", p "$%02X", "")
        SAM_IMM16 -> ("#", p "$%04X", "")
    where
        p s = BS.pack$ printf s t
        t = fromJust$ getOperandInt o

codeAssembly' m a o = CodeAssembly$ Assembly' m a o

assemblyToLongJump :: Assembly -> Either Int BS.ByteString -> Int -> [PatchCode]
assemblyToLongJump (asm@Assembly {..}) addr bank = case mnemonic of
    NM_JMP -> jmlCode
    NM_JML -> jmlCode
    NM_JSR -> jsrCode
    NM_JSL -> jslCode
    NM_BRL -> jmlCode
    n -> branchCode n
    where
        opr = case addr of
            Left a -> Opr'Opr (OprLong a)
            Right label -> Opr'Macro label
        jmlCode = map CodeAssembly [ Assembly' NM_JML (StaticSAM AM_LONG) opr ]
        jslCode = map CodeAssembly [ Assembly' NM_JSL (StaticSAM AM_LONG) opr ]
        branchCode n = map CodeAssembly$
            negCode ++
            [ Assembly' NM_JML (StaticSAM AM_LONG) opr
            ]
            where
                negCode
                    | n == NM_BRA = []
                    | otherwise = [ Assembly' (negBranch n) (StaticSAM AM_PCR) (Opr'Opr$ OprRelByte 4) ]
        jsrCode =
            -- per $04 ; 帰ってきたい所をpushする
            -- pea $YYYY : jml ZZZZ ; RTLがある所をpushする
            [ d [0x4B]  -- PHK
            , d [0x62, 0x06, 0x00]  -- PER $06
            , codeAssembly' NM_PEA (StaticSAM AM_ABS) (Opr'Opr$ OprWord (bankRTLlist!!bank - 1))
            , codeAssembly' NM_JML (StaticSAM AM_LONG) opr
            ]
        d = CodeAssembly. assemblyToAssembly'. fromJust. disassembleCode. BB.pack
        bankRTLlist =
            [ 0x804D, 0x90B9, 0x8BB7, 0x943E
            , 0xA6FA, 0x8125, 0x956D, 0x8BF2
            , 0x8E90, 0x84B4, 0x8099, 0x99EB
            , 0xADB4, 0x97A8, 0x8D7F, 0x8911
            ]

negBranch :: Mnemonic -> Mnemonic
negBranch NM_BCS = NM_BCC
negBranch NM_BCC = NM_BCS
negBranch NM_BEQ = NM_BNE
negBranch NM_BNE = NM_BEQ
negBranch NM_BMI = NM_BPL
negBranch NM_BPL = NM_BMI
negBranch NM_BVC = NM_BVS
negBranch NM_BVS = NM_BVC
negBranch x = x

itiziRAMAddressingMode :: SizedAddressingMode
itiziRAMAddressingMode = StaticSAM AM_ABS
itiziRAMOperand, itiziRAM2Operand :: Operand'
itiziRAMOperand = Opr'Macro "!itizi_ram"
itiziRAM2Operand = Opr'Macro "!itizi_ram2"

assemblyToLongAddressing :: Assembly -> BS.ByteString -> [PatchCode]
assemblyToLongAddressing (asm@Assembly {..}) name
    | isFullMnemonic mnemonic && la /= Nothing =
        map CodeAssembly fullCode
    | isFullMnemonic mnemonic && xa /= Nothing =
        map CodeAssembly fullYCode
    | mnemonic == NM_STY && addressingMode == StaticSAM AM_DIRX =
        map CodeAssembly styXCode
    | mnemonic == NM_LDY && addressingMode == StaticSAM AM_DIRX =
        map CodeAssembly ldyXCode
    | otherwise =
        longCode
    where
        la = StaticSAM <$> (toLongAddressing =<< toStaticSizedAM addressingMode)
        aa = fromMaybe (error$ "assemblyToLongAddressing: unsupported instruction: "++show asm)$
            StaticSAM <$> (toAbsAddressing =<< toStaticSizedAM addressingMode)
        xa
            | addressingMode == StaticSAM AM_DIRY = Just$ StaticSAM AM_LONGX
            | addressingMode == StaticSAM AM_ABSY = Just$ StaticSAM AM_LONGX
            | otherwise = Nothing
        fullCode =
            [ Assembly' mnemonic (fromJust la) (Opr'Macro name)
            ]
        fullYCode =
            [ Assembly' NM_STX itiziRAMAddressingMode itiziRAMOperand
            , d [0x08], d [0xBB], d [0x28]  -- PHP : TYX : PLP
            , Assembly' mnemonic (fromJust xa) (Opr'Macro name)
            , d [0x08]
            , Assembly' NM_LDX itiziRAMAddressingMode itiziRAMOperand
            , d [0x28]
            ]
        styXCode =
            [ Assembly' NM_STA itiziRAMAddressingMode itiziRAMOperand
            , d [0x08], d [0x98], d [0x28]  -- PHP : TYA : PLP
            , Assembly' NM_STA (fromJust la) (Opr'Macro name)
            , d [0x08]
            , Assembly' NM_LDA itiziRAMAddressingMode itiziRAMOperand
            , d [0x28]
            ]
        ldyXCode =
            [ Assembly' NM_STA itiziRAMAddressingMode itiziRAMOperand
            , Assembly' NM_LDA (fromJust la) (Opr'Macro name)
            , d [0xA8]  -- TAY ; どうせフラグ変化はLDAのものと同じなのでPHP : PLP必要ない
            , d [0x08]
            , Assembly' NM_LDA itiziRAMAddressingMode itiziRAMOperand
            , d [0x28]
            ]
        longCode =
            [ e [0x8B], e [0x08]    -- PHB : PHP
            , CodeRaw (BS.concat ["db $F4 : dw ", name, ">>8"]) 3
                -- PEA !name>>8
            , e [0xAB], e [0xAB], e [0x28]  -- PLB : PLB : PLP
            , codeAssembly' mnemonic aa (Opr'Macro name)
            , e [0x08] -- PHP
            , e [0xE2, 0x20]    -- SEP #$20
            , codeAssembly' NM_STA itiziRAMAddressingMode itiziRAMOperand   -- STA !itizi_ram
            , e [0x68]  -- PLA
            , codeAssembly' NM_STA itiziRAMAddressingMode itiziRAM2Operand  -- STA !itizi_ram2
            , e [0xAB]  -- PLB
            , codeAssembly' NM_LDA itiziRAMAddressingMode itiziRAM2Operand  -- LDA !itizi_ram2
            , e [0x48]  -- PHA
            , codeAssembly' NM_LDA itiziRAMAddressingMode itiziRAMOperand   -- LDA !itizi_ram
            , e [0x28]  -- PLP
            ]
        d = assemblyToAssembly'. fromJust. disassembleCode. BB.pack
        e = CodeAssembly. d

toStaticSizedAM :: SizedAddressingMode -> Maybe StaticSizedAM
toStaticSizedAM (StaticSAM x) = Just x
toStaticSizedAM (SizedSAM _) = Nothing

toAbsAddressing, toLongAddressing :: StaticSizedAM -> Maybe StaticSizedAM
toLongAddressing AM_DIR = Just AM_LONG
toLongAddressing AM_DIRX = Just AM_LONGX
toLongAddressing AM_ABS = Just AM_LONG
toLongAddressing AM_ABSX = Just AM_LONGX
toLongAddressing AM_LONG = Just AM_LONG
toLongAddressing AM_LONGX = Just AM_LONGX
toLongAddressing _ = Nothing

toAbsAddressing AM_DIR = Just AM_ABS
toAbsAddressing AM_DIRX = Just AM_ABSX
toAbsAddressing AM_DIRY = Just AM_ABSY
toAbsAddressing AM_ABS = Just AM_ABS
toAbsAddressing AM_ABSX = Just AM_ABSX
toAbsAddressing AM_ABSY = Just AM_ABSY
toAbsAddressing AM_DIRI = Just AM_ABSI
toAbsAddressing AM_ABSI = Just AM_ABSI
toAbsAddressing AM_DIRXI = Just AM_ABSXI
toAbsAddressing AM_ABSXI = Just AM_ABSXI
toAbsAddressing AM_DIRIL = Just AM_ABSIL
toAbsAddressing AM_ABSIL = Just AM_ABSIL
toAbsAddressing _ = Nothing

isFullMnemonic :: Mnemonic -> Bool
isFullMnemonic NM_LDA = True
isFullMnemonic NM_STA = True
isFullMnemonic NM_ORA = True
isFullMnemonic NM_AND = True
isFullMnemonic NM_EOR = True
isFullMnemonic NM_ADC = True
isFullMnemonic NM_CMP = True
isFullMnemonic NM_SBC = True
isFullMnemonic _ = False

assemblyToAssembly' :: Assembly -> Assembly'
assemblyToAssembly' (Assembly {..}) = Assembly'
    { mnemonic' = mnemonic
    , addressingMode' = addressingMode
    , operand' = Opr'Opr operand
    }

data PatchCode = CodeAssembly Assembly' | CodeDB BB.ByteString | CodeRaw BS.ByteString Int
    deriving (Show)

data Assembly' = Assembly'
    { mnemonic' :: Mnemonic
    , addressingMode' :: SizedAddressingMode
    , operand' :: Operand'
    }
    deriving (Show)

data Operand' = Opr'Opr Operand | Opr'Macro BS.ByteString
    deriving (Show)

-- いろいろ

getJumpAddress :: Int -> Assembly -> Maybe Int
getJumpAddress a asm@(Assembly {..}) =
    rel addressingMode <|> abs mnemonic
    where
        rel (StaticSAM AM_PCR) = Just (a+2+x)
        rel (StaticSAM AM_PCRL) = Just (a+3+x)
        rel _ = Nothing
        x = case operand of
            OprRelByte t -> t
            OprRelWord t -> t
            _ -> error$ "getJumpAddress: "++show asm
        abs NM_JMP = short
        abs NM_JSR = short
        abs NM_JML = long
        abs NM_JSL = long
        abs _ = Nothing
        short = case addressingMode of
            StaticSAM AM_ABS -> Just$ (a .&. 0xFF0000) .|. getOperandExpectWord operand
            _ -> Nothing
        long = case addressingMode of
            StaticSAM AM_LONG -> Just$ getOperandExpectLong operand
            _ -> Nothing

getOperandExpectWord, getOperandExpectLong :: Operand -> Int
getOperandExpectWord (OprWord x) = x
getOperandExpectWord o = errorGetOperandExpect "Word" o
getOperandExpectLong (OprLong x) = x
getOperandExpectLong o = errorGetOperandExpect "Long" o

errorGetOperandExpect s o =
    error$ "getOperandExpect"++s++": "++show o

getOperandInt :: Operand -> Maybe Int
getOperandInt (OprByte x) = Just x
getOperandInt (OprRelByte x) = Just x
getOperandInt (OprWord x) = Just x
getOperandInt (OprRelWord x) = Just x
getOperandInt (OprLong x) = Just x
getOperandInt _ = Nothing

isMemoryAccessAddressing :: SizedAddressingMode -> Bool
isMemoryAccessAddressing (StaticSAM a) = case a of
     AM_DIR -> True
     AM_DIRS -> True
     AM_DIRX -> True
     AM_DIRY -> True
     AM_ABS -> True
     AM_ABSX -> True
     AM_ABSY -> True
     AM_LONG -> True
     AM_LONGX -> True
     AM_DIRI -> True
     AM_DIRIY -> True
     AM_DIRSIY -> True
     AM_DIRXI -> True
     AM_ABSI -> True
     AM_ABSXI -> True
     AM_ABSIL -> True
     AM_DIRIL -> True
     AM_DIRILY -> True
     _ -> False
isMemoryAccessAddressing (SizedSAM _) = False


toSigned8 :: Int -> Int
toSigned16 :: Int -> Int
toSigned8 x
    | x >= 0x80 = - (x `xor` 0xff) - 1
    | otherwise = x
toSigned16 x
    | x >= 0x8000 = - (x `xor` 0xffff) - 1
    | otherwise = x

-- Code
type Code = BB.ByteString
type Byte = Int -- うーん…？

codeIndex :: Code -> Int -> Byte
codeIndex x i = fromIntegral (BB.index x i)
codeOpecode :: Code -> Int
codeOperand1 :: Code -> Int
codeOperand1Singed :: Code -> Int
codeOperand2 :: Code -> Int
codeOperand2Signed :: Code -> Int
codeOperand3 :: Code -> Int
codeOpecode x = codeIndex x 0
codeOperand1 x = codeIndex x 1
codeOperand12 x = codeIndex x 2
codeOperand1Singed x = toSigned8 (codeOperand1 x)
codeOperand2 x =
    let x1 = codeIndex x 1 in
    let x2 = codeIndex x 2 in
    fromIntegral x1 .|. (fromIntegral x2 `shiftL` 8)
codeOperand2Signed x = toSigned16 (codeOperand2 x)
codeOperand3 x =
    let x1 = codeIndex x 1 in
    let x2 = codeIndex x 2 in
    let x3 = codeIndex x 3 in
    fromIntegral x1 .|. (fromIntegral x2 `shiftL` 8) .|. (fromIntegral x3 `shiftL` 16)

codeLength :: Code -> Int
codeLength = BB.length

-- Decode
disassembleCode :: Code -> Maybe Assembly
disassembleCode c = do
    (mnem, addressing) <- decodeCode c
    opr <- getOperand addressing c
    return$ Assembly mnem addressing opr

getOperand :: SizedAddressingMode -> Code -> Maybe Operand
getOperand a c =
    guard (getSizedAddressingModeSize a == codeLength c-1) >>
    return ((case a of
    StaticSAM a' -> case a' of
        AM_NONE -> const OprNone
        AM_DIR -> oprByte
        AM_IMM8 -> oprByte
        AM_PCR -> OprRelByte. codeOperand1Singed
        AM_RAW -> \t-> Opr2Byte (codeOperand1 t) (codeOperand12 t)
        AM_DIRS -> oprByte
        AM_DIRX -> oprByte
        AM_DIRY -> oprByte
        AM_ABS -> oprWord
        AM_PCRL -> OprRelWord. codeOperand2Signed
        AM_ABSX -> oprWord
        AM_ABSY -> oprWord
        AM_LONG -> oprLong
        AM_LONGX -> oprLong
        AM_DIRI -> oprByte
        AM_DIRIY -> oprByte
        AM_DIRSIY -> oprByte
        AM_DIRXI -> oprByte
        AM_ABSI -> oprWord
        AM_ABSXI -> oprWord
        AM_ABSIL -> oprWord
        AM_DIRIL -> oprByte
        AM_DIRILY -> oprByte
        AM_ACC -> const OprNone
    SizedSAM a' -> case a' of
        SAM_IMM8 -> oprByte
        SAM_IMM16 -> oprWord) c)
    where
        oprByte = OprByte. codeOperand1
        oprWord = OprWord. codeOperand2
        oprLong = OprLong. codeOperand3

decodeCode :: Code -> Maybe (Mnemonic, SizedAddressingMode)
decodeCode c = do
    guard$ codeLength c >= 1
    let op = codeOpecode c
    let mnem = getMnemonic op
    addressing <- getSizedAddressingModeWithCodeLength op (codeLength c)
    return$ (mnem, addressing)

getMnemonic :: Byte -> Mnemonic
getMnemonic = (mnemonicMap Array.!)

getSizedAddressingModeWithCodeLength :: Byte -> Int -> Maybe SizedAddressingMode
getSizedAddressingModeWithCodeLength b l =
    case addressingModeMap Array.! b of
    StaticAM s -> return$ StaticSAM s
    DynamicAM _ -> case l of
        2 -> return$ SizedSAM SAM_IMM8
        3 -> return$ SizedSAM SAM_IMM16
        _ -> Nothing

getSizedAddressingModeSize :: SizedAddressingMode -> Int
getSizedAddressingModeSize a = case a of
    StaticSAM a' -> case a' of
        AM_NONE -> 0
        AM_DIR -> 1
        AM_IMM8 -> 1
        AM_PCR -> 1
        AM_RAW -> 2
        AM_DIRS -> 1
        AM_DIRX -> 1
        AM_DIRY -> 1
        AM_ABS -> 2
        AM_PCRL -> 2
        AM_ABSX -> 2
        AM_ABSY -> 2
        AM_LONG -> 3
        AM_LONGX -> 3
        AM_DIRI -> 1
        AM_DIRIY -> 1
        AM_DIRSIY -> 1
        AM_DIRXI -> 1
        AM_ABSI -> 2
        AM_ABSXI -> 2
        AM_ABSIL -> 2
        AM_DIRIL -> 1
        AM_DIRILY -> 1
        AM_ACC -> 0
    SizedSAM a' -> case a' of
        SAM_IMM8 -> 1
        SAM_IMM16 -> 2


-- Instruction

mnemonicStrArray :: Array.Array Mnemonic BS.ByteString
mnemonicStrArray = Array.listArray (minBound, maxBound)
    [ "ADC", "AND", "ASL", "BCC"
    , "BCS", "BEQ", "BIT", "BMI"
    , "BNE", "BPL", "BRA", "BRK"
    , "BRL", "BVC", "BVS", "CLC"
    , "CLD", "CLI", "CLV", "CMP"
    , "COP", "CPX", "CPY", "DEC"
    , "DEX", "DEY", "EOR", "INC"
    , "INX", "INY", "JML", "JMP"
    , "JSL", "JSR", "LDA", "LDX"
    , "LDY", "LSR", "MVN", "MVP"
    , "NOP", "ORA", "PEA", "PEI"
    , "PER", "PHA", "PHB", "PHD"
    , "PHK", "PHP", "PHX", "PHY"
    , "PLA", "PLB", "PLD", "PLP"
    , "PLX", "PLY", "REP", "ROL"
    , "ROR", "RTI", "RTL", "RTS"
    , "SBC", "SEC", "SED", "SEI"
    , "SEP", "STA", "STP", "STX"
    , "STY", "STZ", "TAX", "TAY"
    , "TCD", "TCS", "TDC", "TRB"
    , "TSB", "TSC", "TSX", "TXA"
    , "TXY", "TYA", "TYX", "WAI"
    , "WDM", "XBA", "XCE"
    ]

data Assembly = Assembly 
    { mnemonic :: Mnemonic
    , addressingMode :: SizedAddressingMode
    , operand :: Operand
    }
    deriving (Eq, Show)

data Operand =
      OprNone
    | OprByte Int | OprRelByte Int
    | OprWord Int | OprRelWord Int
    | OprLong Int
    | Opr2Byte Int Int
    deriving (Eq, Show)

data Mnemonic =
      NM_ADC | NM_AND | NM_ASL | NM_BCC
    | NM_BCS | NM_BEQ | NM_BIT | NM_BMI
    | NM_BNE | NM_BPL | NM_BRA | NM_BRK
    | NM_BRL | NM_BVC | NM_BVS | NM_CLC
    | NM_CLD | NM_CLI | NM_CLV | NM_CMP
    | NM_COP | NM_CPX | NM_CPY | NM_DEC
    | NM_DEX | NM_DEY | NM_EOR | NM_INC
    | NM_INX | NM_INY | NM_JML | NM_JMP
    | NM_JSL | NM_JSR | NM_LDA | NM_LDX
    | NM_LDY | NM_LSR | NM_MVN | NM_MVP
    | NM_NOP | NM_ORA | NM_PEA | NM_PEI
    | NM_PER | NM_PHA | NM_PHB | NM_PHD
    | NM_PHK | NM_PHP | NM_PHX | NM_PHY
    | NM_PLA | NM_PLB | NM_PLD | NM_PLP
    | NM_PLX | NM_PLY | NM_REP | NM_ROL
    | NM_ROR | NM_RTI | NM_RTL | NM_RTS
    | NM_SBC | NM_SEC | NM_SED | NM_SEI
    | NM_SEP | NM_STA | NM_STP | NM_STX
    | NM_STY | NM_STZ | NM_TAX | NM_TAY
    | NM_TCD | NM_TCS | NM_TDC | NM_TRB
    | NM_TSB | NM_TSC | NM_TSX | NM_TXA
    | NM_TXY | NM_TYA | NM_TYX | NM_WAI
    | NM_WDM | NM_XBA | NM_XCE
    deriving (Eq, Show, Enum, Ord, Array.Ix, Bounded)

-- アドレッシングモード
data StaticSizedAM =
      AM_NONE | AM_DIR | AM_IMM8 | AM_PCR
    | AM_RAW | AM_DIRS | AM_DIRX | AM_DIRY
    | AM_ABS | AM_PCRL | AM_ABSX | AM_ABSY
    | AM_LONG | AM_LONGX | AM_DIRI | AM_DIRIY
    | AM_DIRSIY | AM_DIRXI | AM_ABSI | AM_ABSXI
    | AM_ABSIL | AM_DIRIL | AM_DIRILY | AM_ACC
    deriving (Eq, Show)
data DynamicSizedAM = AM_IMMA | AM_IMMXY
    deriving (Eq, Show)
-- DynamicSizedAMにフラグが与えられることによってSizedAMが決まる
data SizedAM = SAM_IMM8 | SAM_IMM16
    deriving (Eq, Show)
data AddressingMode =
    StaticAM StaticSizedAM | DynamicAM DynamicSizedAM
    deriving (Eq, Show)
data SizedAddressingMode =
    StaticSAM StaticSizedAM | SizedSAM SizedAM
    deriving (Eq, Show)

mnemonicMap :: Array.Array Int Mnemonic
mnemonicMap = Array.listArray (0, 0xff)
    [ NM_BRK, NM_ORA, NM_COP, NM_ORA
    , NM_TSB, NM_ORA, NM_ASL, NM_ORA
    , NM_PHP, NM_ORA, NM_ASL, NM_PHD
    , NM_TSB, NM_ORA, NM_ASL, NM_ORA
    , NM_BPL, NM_ORA, NM_ORA, NM_ORA
    , NM_TRB, NM_ORA, NM_ASL, NM_ORA
    , NM_CLC, NM_ORA, NM_INC, NM_TCS
    , NM_TRB, NM_ORA, NM_ASL, NM_ORA
    , NM_JSR, NM_AND, NM_JSL, NM_AND
    , NM_BIT, NM_AND, NM_ROL, NM_AND
    , NM_PLP, NM_AND, NM_ROL, NM_PLD
    , NM_BIT, NM_AND, NM_ROL, NM_AND
    , NM_BMI, NM_AND, NM_AND, NM_AND
    , NM_BIT, NM_AND, NM_ROL, NM_AND
    , NM_SEC, NM_AND, NM_DEC, NM_TSC
    , NM_BIT, NM_AND, NM_ROL, NM_AND
    , NM_RTI, NM_EOR, NM_WDM, NM_EOR
    , NM_MVP, NM_EOR, NM_LSR, NM_EOR
    , NM_PHA, NM_EOR, NM_LSR, NM_PHK
    , NM_JMP, NM_EOR, NM_LSR, NM_EOR
    , NM_BVC, NM_EOR, NM_EOR, NM_EOR
    , NM_MVN, NM_EOR, NM_LSR, NM_EOR
    , NM_CLI, NM_EOR, NM_PHY, NM_TCD
    , NM_JML, NM_EOR, NM_LSR, NM_EOR
    , NM_RTS, NM_ADC, NM_PER, NM_ADC
    , NM_STZ, NM_ADC, NM_ROR, NM_ADC
    , NM_PLA, NM_ADC, NM_ROR, NM_RTL
    , NM_JMP, NM_ADC, NM_ROR, NM_ADC
    , NM_BVS, NM_ADC, NM_ADC, NM_ADC
    , NM_STZ, NM_ADC, NM_ROR, NM_ADC
    , NM_SEI, NM_ADC, NM_PLY, NM_TDC
    , NM_JMP, NM_ADC, NM_ROR, NM_ADC
    , NM_BRA, NM_STA, NM_BRL, NM_STA
    , NM_STY, NM_STA, NM_STX, NM_STA
    , NM_DEY, NM_BIT, NM_TXA, NM_PHB
    , NM_STY, NM_STA, NM_STX, NM_STA
    , NM_BCC, NM_STA, NM_STA, NM_STA
    , NM_STY, NM_STA, NM_STX, NM_STA
    , NM_TYA, NM_STA, NM_TAX, NM_TXY
    , NM_STZ, NM_STA, NM_STZ, NM_STA
    , NM_LDY, NM_LDA, NM_LDX, NM_LDA
    , NM_LDY, NM_LDA, NM_LDX, NM_LDA
    , NM_TAY, NM_LDA, NM_TAX, NM_PLB
    , NM_LDY, NM_LDA, NM_LDX, NM_LDA
    , NM_BCS, NM_LDA, NM_LDA, NM_LDA
    , NM_LDY, NM_LDA, NM_LDX, NM_LDA
    , NM_CLV, NM_LDA, NM_TSX, NM_TYX
    , NM_LDY, NM_LDA, NM_LDX, NM_LDA
    , NM_CPY, NM_CMP, NM_REP, NM_CMP
    , NM_CPY, NM_CMP, NM_DEC, NM_CMP
    , NM_INY, NM_CMP, NM_DEX, NM_WAI
    , NM_CPY, NM_CMP, NM_DEC, NM_CMP
    , NM_BNE, NM_CMP, NM_CMP, NM_CMP
    , NM_PEI, NM_CMP, NM_DEC, NM_CMP
    , NM_CLD, NM_CMP, NM_PHX, NM_STP
    , NM_JML, NM_CMP, NM_DEC, NM_CMP
    , NM_CPX, NM_SBC, NM_SEP, NM_SBC
    , NM_CPX, NM_SBC, NM_INC, NM_SBC
    , NM_INX, NM_SBC, NM_NOP, NM_XBA
    , NM_CPX, NM_SBC, NM_INC, NM_SBC
    , NM_BEQ, NM_SBC, NM_SBC, NM_SBC
    , NM_PEA, NM_SBC, NM_INC, NM_SBC
    , NM_SED, NM_SBC, NM_PLX, NM_XCE
    , NM_JSR, NM_SBC, NM_INC, NM_SBC
    ]

addressingModeMap :: Array.Array Int AddressingMode
addressingModeMap = Array.listArray (0, 0xff)
    [ StaticAM AM_IMM8, StaticAM AM_DIRXI, StaticAM AM_IMM8, StaticAM AM_DIRS
    , StaticAM AM_DIR, StaticAM AM_DIR, StaticAM AM_DIR, StaticAM AM_DIRIL
    , StaticAM AM_NONE, DynamicAM AM_IMMA, StaticAM AM_ACC, StaticAM AM_NONE
    , StaticAM AM_ABS, StaticAM AM_ABS, StaticAM AM_ABS, StaticAM AM_LONG
    , StaticAM AM_PCR, StaticAM AM_DIRIY, StaticAM AM_DIRI, StaticAM AM_DIRSIY
    , StaticAM AM_DIR, StaticAM AM_DIRX, StaticAM AM_DIRX, StaticAM AM_DIRILY
    , StaticAM AM_NONE, StaticAM AM_ABSY, StaticAM AM_ACC, StaticAM AM_NONE
    , StaticAM AM_ABS, StaticAM AM_ABSX, StaticAM AM_ABSX, StaticAM AM_LONGX
    , StaticAM AM_ABS, StaticAM AM_DIRXI, StaticAM AM_LONG, StaticAM AM_DIRS
    , StaticAM AM_DIR, StaticAM AM_DIR, StaticAM AM_DIR, StaticAM AM_DIRIL
    , StaticAM AM_NONE, DynamicAM AM_IMMA, StaticAM AM_ACC, StaticAM AM_NONE
    , StaticAM AM_ABS, StaticAM AM_ABS, StaticAM AM_ABS, StaticAM AM_LONG
    , StaticAM AM_PCR, StaticAM AM_DIRIY, StaticAM AM_DIRI, StaticAM AM_DIRSIY
    , StaticAM AM_DIRX, StaticAM AM_DIRX, StaticAM AM_DIRX, StaticAM AM_DIRILY
    , StaticAM AM_NONE, StaticAM AM_ABSY, StaticAM AM_ACC, StaticAM AM_NONE
    , StaticAM AM_ABSX, StaticAM AM_ABSX, StaticAM AM_ABSX, StaticAM AM_LONGX
    , StaticAM AM_NONE, StaticAM AM_DIRXI, StaticAM AM_NONE, StaticAM AM_DIRS
    , StaticAM AM_RAW, StaticAM AM_DIR, StaticAM AM_DIR, StaticAM AM_DIRIL
    , StaticAM AM_NONE, DynamicAM AM_IMMA, StaticAM AM_ACC, StaticAM AM_NONE
    , StaticAM AM_ABS, StaticAM AM_ABS, StaticAM AM_ABS, StaticAM AM_LONG
    , StaticAM AM_PCR, StaticAM AM_DIRIY, StaticAM AM_DIRI, StaticAM AM_DIRSIY
    , StaticAM AM_RAW, StaticAM AM_DIRX, StaticAM AM_DIRX, StaticAM AM_DIRILY
    , StaticAM AM_NONE, StaticAM AM_ABSX, StaticAM AM_NONE, StaticAM AM_NONE
    , StaticAM AM_LONG, StaticAM AM_ABSX, StaticAM AM_ABSX, StaticAM AM_LONGX
    , StaticAM AM_NONE, StaticAM AM_DIRXI, StaticAM AM_ABS, StaticAM AM_DIRS
    , StaticAM AM_DIR, StaticAM AM_DIR, StaticAM AM_DIR, StaticAM AM_DIRIL
    , StaticAM AM_NONE, DynamicAM AM_IMMA, StaticAM AM_ACC, StaticAM AM_NONE
    , StaticAM AM_ABSI, StaticAM AM_ABS, StaticAM AM_ABS, StaticAM AM_LONG
    , StaticAM AM_PCR, StaticAM AM_DIRIY, StaticAM AM_DIRI, StaticAM AM_DIRSIY
    , StaticAM AM_DIRX, StaticAM AM_DIRX, StaticAM AM_DIRX, StaticAM AM_DIRILY
    , StaticAM AM_NONE, StaticAM AM_ABSY, StaticAM AM_NONE, StaticAM AM_NONE
    , StaticAM AM_ABSXI, StaticAM AM_ABSX, StaticAM AM_ABSX, StaticAM AM_LONGX
    , StaticAM AM_PCR, StaticAM AM_DIRXI, StaticAM AM_PCRL, StaticAM AM_DIRS
    , StaticAM AM_DIR, StaticAM AM_DIR, StaticAM AM_DIR, StaticAM AM_DIRIL
    , StaticAM AM_NONE, DynamicAM AM_IMMA, StaticAM AM_NONE, StaticAM AM_NONE
    , StaticAM AM_ABS, StaticAM AM_ABS, StaticAM AM_ABS, StaticAM AM_LONG
    , StaticAM AM_PCR, StaticAM AM_DIRIY, StaticAM AM_DIRI, StaticAM AM_DIRSIY
    , StaticAM AM_DIRX, StaticAM AM_DIRX, StaticAM AM_DIRY, StaticAM AM_DIRILY
    , StaticAM AM_NONE, StaticAM AM_ABSY, StaticAM AM_NONE, StaticAM AM_NONE
    , StaticAM AM_ABS, StaticAM AM_ABSX, StaticAM AM_ABSX, StaticAM AM_LONGX
    , DynamicAM AM_IMMXY, StaticAM AM_DIRXI, DynamicAM AM_IMMXY, StaticAM AM_DIRS
    , StaticAM AM_DIR, StaticAM AM_DIR, StaticAM AM_DIR, StaticAM AM_DIRIL
    , StaticAM AM_NONE, DynamicAM AM_IMMA, StaticAM AM_NONE, StaticAM AM_NONE
    , StaticAM AM_ABS, StaticAM AM_ABS, StaticAM AM_ABS, StaticAM AM_LONG
    , StaticAM AM_PCR, StaticAM AM_DIRIY, StaticAM AM_DIRI, StaticAM AM_DIRSIY
    , StaticAM AM_DIRX, StaticAM AM_DIRX, StaticAM AM_DIRY, StaticAM AM_DIRILY
    , StaticAM AM_NONE, StaticAM AM_ABSY, StaticAM AM_NONE, StaticAM AM_NONE
    , StaticAM AM_ABSX, StaticAM AM_ABSX, StaticAM AM_ABSY, StaticAM AM_LONGX
    , DynamicAM AM_IMMXY, StaticAM AM_DIRXI, StaticAM AM_IMM8, StaticAM AM_DIRS
    , StaticAM AM_DIR, StaticAM AM_DIR, StaticAM AM_DIR, StaticAM AM_DIRIL
    , StaticAM AM_NONE, DynamicAM AM_IMMA, StaticAM AM_NONE, StaticAM AM_NONE
    , StaticAM AM_ABS, StaticAM AM_ABS, StaticAM AM_ABS, StaticAM AM_LONG
    , StaticAM AM_PCR, StaticAM AM_DIRIY, StaticAM AM_DIRI, StaticAM AM_DIRSIY
    , StaticAM AM_DIRI, StaticAM AM_DIRX, StaticAM AM_DIRX, StaticAM AM_DIRILY
    , StaticAM AM_NONE, StaticAM AM_ABSY, StaticAM AM_NONE, StaticAM AM_NONE
    , StaticAM AM_ABSIL, StaticAM AM_ABSX, StaticAM AM_ABSX, StaticAM AM_LONGX
    , DynamicAM AM_IMMXY, StaticAM AM_DIRXI, StaticAM AM_IMM8, StaticAM AM_DIRS
    , StaticAM AM_DIR, StaticAM AM_DIR, StaticAM AM_DIR, StaticAM AM_DIRIL
    , StaticAM AM_NONE, DynamicAM AM_IMMA, StaticAM AM_NONE, StaticAM AM_NONE
    , StaticAM AM_ABS, StaticAM AM_ABS, StaticAM AM_ABS, StaticAM AM_LONG
    , StaticAM AM_PCR, StaticAM AM_DIRIY, StaticAM AM_DIRI, StaticAM AM_DIRSIY
    , StaticAM AM_ABS, StaticAM AM_DIRX, StaticAM AM_DIRX, StaticAM AM_DIRILY
    , StaticAM AM_NONE, StaticAM AM_ABSY, StaticAM AM_NONE, StaticAM AM_NONE
    , StaticAM AM_ABSXI, StaticAM AM_ABSX, StaticAM AM_ABSX, StaticAM AM_LONGX
    ]
