{-# LANGUAGE RecordWildCards #-}

module Disassemble
    ( Code, Assembly (..), Operand (..), CodeBytes (..)
    , disassembleCode
    , isMemoryAccessAddressing, getOpearndInt
    , getJumpAddress
    ) where

import qualified Data.ByteString as BB
import Data.Bits
import qualified Data.Array as Array
import Control.Monad
import Control.Applicative

data CodeBytes = CodeAssembly Assembly | CodeDB BB.ByteString

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

getOperandExpectByte, getOperandExpectWord
    , getOperandExpectLong :: Operand -> Int
getOperandExpectByte (OprByte x) = x
getOperandExpectByte o = errorGetOperandExpect "Byte" o
getOperandExpectWord (OprWord x) = x
getOperandExpectWord o = errorGetOperandExpect "Word" o
getOperandExpectLong (OprLong x) = x
getOperandExpectLong o = errorGetOperandExpect "Long" o

errorGetOperandExpect s o =
    error$ "getOperandExpect"++s++": "++show o

getOpearndInt :: Operand -> Maybe Int
getOpearndInt (OprByte x) = Just x
getOpearndInt (OprRelByte x) = Just x
getOpearndInt (OprWord x) = Just x
getOpearndInt (OprRelWord x) = Just x
getOpearndInt (OprLong x) = Just x
getOpearndInt _ = Nothing

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
        AM_RAW -> \c-> Opr2Byte (codeOperand1 c) (codeOperand12 c)
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
    DynamicAM d -> case l of
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

data Assembly = Assembly 
    { mnemonic :: Mnemonic
    , addressingMode :: SizedAddressingMode
    , operand :: Operand
    }
    deriving (Show)

data Operand =
      OprNone
    | OprByte Int | OprRelByte Int
    | OprWord Int | OprRelWord Int
    | OprLong Int
    | Opr2Byte Int Int
    deriving (Show)

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
    deriving (Show)

-- アドレッシングモード
data StaticSizedAM =
      AM_NONE | AM_DIR | AM_IMM8 | AM_PCR
    | AM_RAW | AM_DIRS | AM_DIRX | AM_DIRY
    | AM_ABS | AM_PCRL | AM_ABSX | AM_ABSY
    | AM_LONG | AM_LONGX | AM_DIRI | AM_DIRIY
    | AM_DIRSIY | AM_DIRXI | AM_ABSI | AM_ABSXI
    | AM_ABSIL | AM_DIRIL | AM_DIRILY | AM_ACC
    deriving (Show)
data DynamicSizedAM = AM_IMMA | AM_IMMXY
    deriving (Show)
-- DynamicSizedAMにフラグが与えられることによってSizedAMが決まる
data SizedAM = SAM_IMM8 | SAM_IMM16
    deriving (Show)
data AddressingMode =
    StaticAM StaticSizedAM | DynamicAM DynamicSizedAM
    deriving (Show)
data SizedAddressingMode =
    StaticSAM StaticSizedAM | SizedSAM SizedAM
    deriving (Show)

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
