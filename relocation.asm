header
lorom
;##############################
; 複数のオフセットで使われてる奴は面倒だからやめよう


;##############################
; 実際にhijackするもの

;==============================
;
unknown_0af6 = $7EC100
org $01CCBF
        lda.w unknown_0af6,y
org $03D9AE
        sta.w unknown_0af6,y
org $0480C5
        sta.w unknown_0af6,x
org $0480D5
        sta.w unknown_0af6,x
org $0480E8
        lda.w unknown_0af6,x
org $0480F4
        rol.w unknown_0af6,x
org $0480FB
        ror.w unknown_0af6,x
org $04817D
        lda.w unknown_0af6,x
org $048183
        lda.w unknown_0af6,x
org $04818A
        sta.w unknown_0af6,x
org $0CA07D
        sta.w unknown_0af6,x
org $0CA42B
        sta.w unknown_0af6,x
org $0CA8A9
        adc.w unknown_0af6,x
org $0CA8C3
        sta.w unknown_0af6,x


C:\test\game\snes\smw\relocation>SMWDisC_find $0AF6 unkno
;==============================
;
unknown_0af6 = $0AF6
org $01CCBF
        jml HIJACK_01CCBF
org $03D9AE
        jml HIJACK_03D9AE
org $0480C5
        jml HIJACK_0480C5
org $0480D5
        jml HIJACK_0480D5
org $0480E8
        jml HIJACK_0480E8
org $0480F4
        jml HIJACK_0480F4
org $0480FB
        jml HIJACK_0480FB
org $04817D
        jml HIJACK_04817D
org $048183
        jml HIJACK_048183
org $04818A
        jml HIJACK_04818A
org $0CA07D
        jml HIJACK_0CA07D
org $0CA42B
        jml HIJACK_0CA42B
org $0CA8A9
        jml HIJACK_0CA8A9
org $0CA8C3
        jml HIJACK_0CA8C3



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
HIJACK_01CCBF:
        PHX : TYX : lda.l unknown_0af6,x : PLX
        db $C9, $15
        jml $01ccc4
HIJACK_03D9AE:
        PHX : TYX : sta.l unknown_0af6,x : PLX
        db $0A
        jml $03d9b2
HIJACK_0480C5:
        sta.l unknown_0af6,x
        db $C8
        jml $0480c9
HIJACK_0480D5:
        sta.l unknown_0af6,x
        db $C8
        jml $0480d9
HIJACK_0480E8:
        lda.l unknown_0af6,x
        db $85, $00
        jml $0480ed
HIJACK_0480F4:
        rol.l unknown_0af6,x
        db $80, $05
        jml $0480f9
HIJACK_0480FB:
        ror.l unknown_0af6,x
        db $CA
        jml $0480ff
HIJACK_04817D:
        lda.l unknown_0af6,x
        db $85, $00
        jml $048182
HIJACK_048183:
        lda.l unknown_0af6,x
        db $85, $02
        jml $048188
HIJACK_04818A:
        sta.l unknown_0af6,x
        db $A5, $02
        jml $04818f
HIJACK_0CA07D:
        sta.l unknown_0af6,x
        db $88
        jml $0ca081
HIJACK_0CA42B:
        sta.l unknown_0af6,x
        db $9D, $6E, $0B
        jml $0ca431
HIJACK_0CA8A9:
        adc.l unknown_0af6,x
        db $9D, $05, $0B
        jml $0ca8af
HIJACK_0CA8C3:
        sta.l unknown_0af6,x
        db $A5, $0E
        jml $0ca8c8





























CodeEnd:
	print "pc: ", pc
