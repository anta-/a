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

!SpriteNum = $0FBE
!SpriteYSpeed = $0FCA
!SpriteXSpeed = $0FD6
!SpriteState = $0FE2
!SpritePosYLo = $0FEE
!SpritePosXLo = $0FFA
!SpriteAction = $1006
!SpriteYHi = $1012
!SpriteXHi = $101E
!SprTbl14EC = $102A
!SprTbl14F8 = $1036
!SprTbl1504 = $1042
!SprTbl1510 = $104E
!SprTbl151C = $105A
!SprTbl1528 = $1066
!SprTbl1534 = $1072
!SprTbl1540 = $107E
!DisableInter = $108A
!SprTbl1558 = $1096
!SprTbl1564 = $10A2
!SprTbl1570 = $10AE
!SpriteDir = $10BA
!SprObjStatus = $10C6
!SprTbl1594 = $10D2
!OffscreenHorz = $10DE
!SprTbl15AC = $10EA
!SprTbl15B8 = $10F6
!SprTbl15C4 = $1102
!SprTbl15D0 = $110E
!SprTbl15DC = $111A
!SprOAMIndex = $1126
!SpritePal = $1132
!SprTbl1602 = $113E
!SprTbl160E = $114A
!SprIndexInLvl = $1156
!SprTbl1626 = $1162
!SprBehindScrn = $116E
!SprTbl163E = $117A
!SprTbl164A = $1186
!Tweaker1656 = $1192
!Tweaker1662 = $119E
!Tweaker166E = $11AA
!Tweaker167A = $11B6
!Tweaker1686 = $11C2
!OffscreenVert = $11CE
!SprTbl187B = $11DA
!Tweaker190F = $11E6
!SprTbl1FD6 = $11F2
!SprTbl1FE2 = $11FE

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

print "END: ", pc
