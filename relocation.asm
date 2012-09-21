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
!SpriteState2 = $14C8
incsrc relocationPatch.asm

;##############################
; hijack用の空き領域
!CodeOffset = $108000

!RatsCodeSize = CodeEnd-!CodeOffset-$08
org !CodeOffset
db "STAR"
dw !RatsCodeSize-$01
dw !RatsCodeSize-$01^$FFFF

db "Hello!"

; ここから

incsrc relocationHijack_0FBE.asm
incsrc relocationHijack.asm

CodeEnd:
	print "pc: ", pc
