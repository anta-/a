header
lorom
;##############################

!itizi_ram = $1DFD	; 2bytes
!itizi_ram2 = $1E00	; 1byte

;##############################
; アドレスだけ変えるもの


;##############################
; hijackするもの

!Pointer16x = $7EC100
incsrc relocationPatch_0FBE.asm

!SpriteNum = $0FBE
!SpriteYSpeed = $0FD2
!SpriteXSpeed = $0FE6
!SpriteState = $0FFA
!SpritePosYLo = $100E
!SpritePosXLo = $1022
!SpriteAction = $1036
!SpriteYHi = $104A
!SpriteXHi = $105E
!SprTbl14EC = $1072
!SprTbl14F8 = $1086
!SprTbl1504 = $109A
!SprTbl1510 = $10AE
!SprTbl151C = $10C2
!SprTbl1528 = $10D6
!SprTbl1534 = $10EA
!SprTbl1540 = $10FE
!DisableInter = $1112
!SprTbl1558 = $1126
!SprTbl1564 = $113A
!SprTbl1570 = $114E
!SpriteDir = $1162
!SprObjStatus = $1176
!SprTbl1594 = $118A
!OffscreenHorz = $119E
!SprTbl15AC = $11B2
!SprTbl15B8 = $11C6
!SprTbl15C4 = $11DA
!SprTbl15D0 = $11EE
!SprTbl15DC = $1202
!SprOAMIndex = $1216
!SpritePal = $122A
!SprTbl1602 = $123E
!SprTbl160E = $1252
!SprIndexInLvl = $1266
!SprTbl1626 = $127A
!SprBehindScrn = $128E
!SprTbl163E = $12A2
!SprTbl164A = $12B6
!Tweaker1656 = $12CA
!Tweaker1662 = $12DE
!Tweaker166E = $12F2
!Tweaker167A = $1306
!Tweaker1686 = $131A
!OffscreenVert = $132E
!SprTbl187B = $1342
!Tweaker190F = $1356
!SprTbl1FD6 = $136A
!SprTbl1FE2 = $137E

!SprTblSize = $14
!SprTblIdxMax = !SprTblSize-1
!SprTblNmrIdxMax = !SprTblIdxMax-2

incsrc relocationPatch.asm

;##############################
; SprTblSizeの修正
org $01ABCE
	ADC.b #!SprTblSize
org $02D28A
	ADC.b #!SprTblSize
; LDX
org $00FA10 : LDX.b #!SprTblIdxMax
org $00FCEC : LDX.b #!SprTblIdxMax
org $0180A7 : LDX.b #!SprTblIdxMax
org $028017 : LDX.b #!SprTblIdxMax
org $028905 : LDX.b #!SprTblIdxMax
org $0293AE : LDX.b #!SprTblIdxMax
org $02ABFE : LDX.b #!SprTblIdxMax
org $03C2DA : LDX.b #!SprTblIdxMax
; LDY
org $00FA8F : LDY.b #!SprTblIdxMax
org $00FC23 : LDY.b #!SprTblIdxMax
org $01C2D3 : LDY.b #!SprTblIdxMax
org $01E1C8 : LDY.b #!SprTblIdxMax
org $01F566 : LDY.b #!SprTblIdxMax
org $01F629 : LDY.b #!SprTblIdxMax
org $0289DF : LDY.b #!SprTblIdxMax
org $02BAB5 : LDY.b #!SprTblIdxMax
org $02E5F7 : LDY.b #!SprTblIdxMax
org $02EA4E : LDY.b #!SprTblIdxMax
org $02EF6E : LDY.b #!SprTblIdxMax
org $0381E4 : LDY.b #!SprTblIdxMax
; SpriteSlotMax
org $02A773
	db !SprTblNmrIdxMax,$05,$07,$07,$07,$06,$07,$06
	db $06,$09,$08,$04,$07,$07,$07,$08
	db $09,$05,$05
	
	db !SprTblNmrIdxMax,$07,$07,$01,$00,$01,$07,$06
	db $06,$00,$02,$00,$07,$01,$07,$08
	db $09,$07,$05
	
	db !SprTblNmrIdxMax,$07,$07,$01,$00,$06,$07,$06
	db $06,$00,$02,$00,$07,$01,$07,$08
	db $09,$07,$05
; スプライトのOAMアドレス
org $07F000
	db $30,$3C,$48,$54,$60,$6C,$78,$84
	db $90,$9C,$A8,$B4,$C0,$CC,$B8,$C4
	db $D0,$DC,$28,$2C

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
