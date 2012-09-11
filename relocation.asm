header
lorom
;##############################
; 複数のオフセットで使われてる奴は面倒だからやめよう
; スプライト系のRAMはぱっと数えただけで 12 * 50 = 0x258 ある。

;##############################
; アドレスだけ変えるもの

incsrc st.asm

;##############################
; hijackするもの

;==============================
; > $7E:0FBE 1024Bytes(400) 16x用16bitポインタ？
; 大きな空きが確保できてバグらないから利用
!pointer_16x = $7EC100
org $00C17C
	jml HIJACK_00C17C
org $00C25E
	jml HIJACK_00C25E
org $04DC44
	jml HIJACK_04DC44
org $0580EC
	jml HIJACK_0580EC
org $0581A7
	jml HIJACK_0581A7
org $058245
	jml HIJACK_058245
org $058257
	jml HIJACK_058257
org $05829F
	jml HIJACK_05829F
org $0582B7
	jml HIJACK_0582B7
org $0587BB
	jml HIJACK_0587BB
org $058A66
	jml HIJACK_058A66
org $058B46
	jml HIJACK_058B46
org $058C34
	jml HIJACK_058C34
org $058D2B
	jml HIJACK_058D2B
org $0C94AD
	jml HIJACK_0C94AD
org $0C94FD
	jml HIJACK_0C94FD


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

;==============================
; 
HIJACK_00C17C:
	PHX : TYX : lda.l !pointer_16x,x : PLX
	db $85, $04
	jml $00c181
HIJACK_00C25E:
	PHX : TYX : lda.l !pointer_16x,x : PLX
	db $85, $04
	jml $00c263
HIJACK_04DC44:
	sta.l !pointer_16x,x
	db $A5, $00
	jml $04dc49
HIJACK_0580EC:
	PHX : TYX : sta.l !pointer_16x,x : PLX
	db $C8
	jml $0580f0
HIJACK_0581A7:
	sta.l !pointer_16x,x
	db $A5, $00
	jml $0581ac
HIJACK_058245:
	sta.l !pointer_16x,x
	db $A5, $02
	jml $05824a
HIJACK_058257:
	sta.l !pointer_16x,x
	db $A5, $00
	jml $05825c
HIJACK_05829F:
	PHX : TYX : sta.l !pointer_16x,x : PLX
	db $18
	jml $0582a3
HIJACK_0582B7:
	PHX : TYX : sta.l !pointer_16x,x : PLX
	db $18
	jml $0582bb
HIJACK_0587BB:
	PHX : TYX : sta.l !pointer_16x,x : PLX
	db $C8
	jml $0587bf
HIJACK_058A66:
	PHX : TYX : lda.l !pointer_16x,x : PLX
	db $85, $0A
	jml $058a6b
HIJACK_058B46:
	PHX : TYX : lda.l !pointer_16x,x : PLX
	db $85, $0A
	jml $058b4b
HIJACK_058C34:
	PHX : TYX : lda.l !pointer_16x,x : PLX
	db $85, $0A
	jml $058c39
HIJACK_058D2B:
	PHX : TYX : lda.l !pointer_16x,x : PLX
	db $85, $0A
	jml $058d30
HIJACK_0C94AD:
	sta.l !pointer_16x,x
	db $A5, $00
	jml $0c94b2
HIJACK_0C94FD:
	lda.l !pointer_16x,x
	db $85, $68
	jml $0c9502


























CodeEnd:
	print "pc: ", pc
