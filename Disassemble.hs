module Disassemble where

import qualified Data.ByteString as BB
import Data.Bits
import qualified Data.Array as Array

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
    fromIntegral (codeIndex x 1) .|.
    (fromIntegral (codeIndex x 2) `shiftL` 8)
codeOperand2Signed x = toSigned16 (codeOperand2 x)
codeOperand3 x =
    fromIntegral (codeIndex x 1) .|.
    (fromIntegral (codeIndex x 2) `shiftL` 8) .|.
    (fromIntegral (codeIndex x 3) `shiftL` 16)

codeLength :: Code -> Int
codeLength = BB.length

-- Decode
disassembleCode :: Code -> Assembly
disassembleCode c =
    let decode@(Decode _ addressing) = decodeCode c in
    Assembly decode (getOperand addressing c)

getOperand :: SizedAddressingMode -> Code -> Operand
getOperand a = case a of
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
        SAM_IMM16 -> oprWord
    where
        oprByte = OprByte. codeOperand1
        oprWord = OprWord. codeOperand2
        oprLong = OprLong. codeOperand3

decodeCode :: Code -> Decode
decodeCode c = Decode
    (getMnemonic op)
    (getSizedAddressingModeWithCodeLength op (codeLength c))
    where op = codeOpecode c

getMnemonic :: Byte -> Mnemonic
getMnemonic = (mnemonicMap Array.!)

getSizedAddressingModeWithCodeLength :: Byte -> Int -> SizedAddressingMode
getSizedAddressingModeWithCodeLength b l =
    case addressingModeMap Array.! b of
    StaticAM s -> StaticSAM s
    DynamicAM d -> SizedSAM$ case l of
        2 -> SAM_IMM8
        3 -> SAM_IMM16
        _ -> error$ "getSizedAddressingModeWithCodeLength: codeLength == " ++ show l

-- Instruction

data Assembly = Assembly Decode Operand
    deriving (Show)

data Operand =
      OprNone
    | OprByte Int | OprRelByte Int
    | OprWord Int | OprRelWord Int
    | OprLong Int
    | Opr2Byte Int Int
    deriving (Show)

data Decode = Decode Mnemonic SizedAddressingMode
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
