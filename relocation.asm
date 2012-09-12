header
lorom
;##############################
; 複数のオフセットで使われてる奴は面倒だからやめよう
; スプライト系のRAMはぱっと数えただけで 12 * 50 = 0x258 ある。

!itizi_ram = $13BA
!itizi_ram2 = $13BC

;##############################
; アドレスだけ変えるもの

!st_start = $0FBE
!st_size = $C

incsrc st.asm

;##############################
; hijackするもの

;==============================
; > $7E:0FBE 1024Bytes(400) 16x用16bitポインタ？
; 大きな空きが確保できてバグらないから利用
!pointer_16x = $7EC100
org $00C102
	jml HIJACK_00C102
org $00C120
	jml HIJACK_00C120
org $00C1E6
	jml HIJACK_00C1E6
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
org $058A43
	jml HIJACK_058A43
org $058A66
	jml HIJACK_058A66
org $058B1F
	jml HIJACK_058B1F
org $058B46
	jml HIJACK_058B46
org $058C11
	jml HIJACK_058C11
org $058C34
	jml HIJACK_058C34
org $058D04
	jml HIJACK_058D04
org $058D8D
	jml HIJACK_058D8D
org $058D2B
	jml HIJACK_058D2B
org $0C94AD
	jml HIJACK_0C94AD
org $0C94FD
	jml HIJACK_0C94FD

incsrc a1.log


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
HIJACK_00C102:
	db $D0, $04 : jml $00C106
	db $46, $00
	jml $00c106
HIJACK_00C120:
	db $D0, $04 : jml $00C124
	db $90, $04 : jml $00C13E
	jml $00c124
HIJACK_00C1E6:
	db $D0, $04 : jml $00C1EA
	db $46, $00
	jml $00c1ea
HIJACK_00C17C:
	stx !itizi_ram : tyx : lda.l !pointer_16x,x : php : ldx !itizi_ram : plp
	db $85, $04
	jml $00c181
HIJACK_00C25E:
	stx !itizi_ram : tyx : lda.l !pointer_16x,x : php : ldx !itizi_ram : plp
	db $85, $04
	jml $00c263
HIJACK_04DC44:
	sta.l !pointer_16x,x
	db $A5, $00
	jml $04dc49
HIJACK_0580EC:
	stx !itizi_ram : tyx : sta.l !pointer_16x,x : php : ldx !itizi_ram : plp
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
	stx !itizi_ram : tyx : sta.l !pointer_16x,x : php : ldx !itizi_ram : plp
	db $18
	jml $0582a3
HIJACK_0582B7:
	stx !itizi_ram : tyx : sta.l !pointer_16x,x : php : ldx !itizi_ram : plp
	db $18
	jml $0582bb
HIJACK_0587BB:
	stx !itizi_ram : tyx : sta.l !pointer_16x,x : php : ldx !itizi_ram : plp
	db $C8
	jml $0587bf
HIJACK_058A43:
	db $10, $04 : jml $058A47
	db $A0, $05
	jml $058a47
HIJACK_058A66:
	stx !itizi_ram : tyx : lda.l !pointer_16x,x : php : ldx !itizi_ram : plp
	db $85, $0A
	jml $058a6b
HIJACK_058B1F:
	db $10, $04 : jml $058B23
	db $A0, $05
	jml $058b23
HIJACK_058B46:
	stx !itizi_ram : tyx : lda.l !pointer_16x,x : php : ldx !itizi_ram : plp
	db $85, $0A
	jml $058b4b
HIJACK_058C11:
	db $10, $04 : jml $058C15
	db $A0, $05
	jml $058c15
HIJACK_058C34:
	stx !itizi_ram : tyx : lda.l !pointer_16x,x : php : ldx !itizi_ram : plp
	db $85, $0A
	jml $058c39
HIJACK_058D04:
	db $10, $04 : jml $058D08
	db $A0, $05
	jml $058d08
HIJACK_058D8D:
	db $D0, $04 : jml $058D91
	db $A0, $34
	jml $058d91
HIJACK_058D2B:
	stx !itizi_ram : tyx : lda.l !pointer_16x,x : php : ldx !itizi_ram : plp
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

;==============================
; 
incsrc a2.log























CodeEnd:
	print "pc: ", pc
