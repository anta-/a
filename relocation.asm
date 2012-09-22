header
lorom
;##############################
; 複数のオフセットで使われてる奴は面倒だからやめよう
; スプライト系のRAMはぱっと数えただけで 12 * 50 = 0x258 ある。

!itizi_ram = $1DFD	; 2bytes
!itizi_ram2 = $1E00	; 1byte

;##############################
; アドレスだけ変えるもの


;##############################
; hijackするもの

!Pointer16x = $7EC100
incsrc relocationPatch_0FBE.asm
!SpriteTableStart = $0FBE
!SpriteTableSize = $C
!SpriteNum = $9E;!SpriteTableSize*0+!SpriteTableStart
!SpriteYSpeed = $AA;!SpriteTableSize*1+!SpriteTableStart
!SpriteXSpeed = $B6;!SpriteTableSize*2+!SpriteTableStart
!SpriteState = $C2;!SpriteTableSize*3+!SpriteTableStart
!SpritePosYLo = $D8;!SpriteTableSize*4+!SpriteTableStart
!SpritePosXLo = $E4;!SpriteTableSize*5+!SpriteTableStart

!SpriteAction = $0FBE
!SpriteYHi = $0FCA
!SpriteXHi = $0FD6
!SprTbl14EC = $0FE2
!SprTbl14F8 = $0FEE
!SprTbl1504 = $0FFA
!SprTbl1510 = $1006
!SprTbl151C = $1012
!SprTbl1528 = $101E
!SprTbl1534 = $102A
!SprTbl1540 = $1036
!DisableInter = $1042
!SprTbl1558 = $104E
!SprTbl1564 = $105A
!SprTbl1570 = $1066
!SpriteDir = $1072
!SprObjStatus = $107E
!SprTbl1594 = $108A
!OffscreenHorz = $1096
!SprTbl15AC = $10A2
!SprTbl15B8 = $10AE
!SprTbl15C4 = $10BA
!SprTbl15D0 = $10C6
!SprTbl15DC = $10D2
!SprOAMIndex = $10DE
!SpritePal = $10EA
!SprTbl1602 = $10F6
!SprTbl160E = $1102
!SprIndexInLvl = $110E
!SprTbl1626 = $111A
!SprBehindScrn = $1126
!SprTbl163E = $1132
!SprTbl164A = $113E
!Tweaker1656 = $114A
!Tweaker1662 = $1156
!Tweaker166E = $1162
!Tweaker167A = $116E
!Tweaker1686 = $117A
!OffscreenVert = $1186
!SprTbl187B = $1192
!Tweaker190F = $119E
!SprTbl1FD6 = $11AA
!SprTbl1FE2 = $11B6

incsrc relocationPatch.asm

;##############################
; hijack用の空き領域
!CodeOffset = $108000
!CodeOffset11 = $118000

org !CodeOffset

db "Hello!"

; ここから

incsrc relocationHijack_0FBE.asm
incsrc relocationHijack.asm

print "pc: ", pc
