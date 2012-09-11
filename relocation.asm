header
lorom

;##############################
; 実際にhijackするもの

;==============================
;> $7E:04A0 448Bytes(1C0)	HDMAテーブル領域
hdma_table = $7EC100
org $00926A
        jml HIJACK_00926A
org $0092B4
        jml HIJACK_0092B4
org $00A185
        jml HIJACK_00A185
org $00CA9F
        jml HIJACK_00CA9F
org $00CAAA
        jml HIJACK_00CAAA
org $00CAD3
        jml HIJACK_00CAD3
org $00CAFB
        jml HIJACK_00CAFB
org $03C609
        jml HIJACK_03C609
org $03C612
        jml HIJACK_03C612
org $0CAB7C
        jml HIJACK_0CAB7C




;##############################
; hijack用の空き領域
!CodeOffset = $108000

!RatsCodeSize = CodeEnd-!CodeOffset-$08
org !CodeOffset
db "STAR"
dw !RatsCodeSize-$01
dw !RatsCodeSize-$01^$FFFF

db "Hello!"

;==============================
;
HIJACK_00926A:
        sta.l hdma_table,x
        db $9E, $A1, $04
        jml $009270
HIJACK_0092B4:
        sta.l hdma_table
        db $8D, $AA, $04
        jml $0092ba
HIJACK_00A185:
        stz.l hdma_table,x
        db $9D, $A1, $04
        jml $00a18b
HIJACK_00CA9F:
        sta.l hdma_table,x
        db $9E, $A1, $04
        jml $00caa5
HIJACK_00CAAA:
        PHX : TYX : sta.l hdma_table,x : PLX
        db $1A
        jml $00caae
HIJACK_00CAD3:
        sta.l hdma_table,x
        db $C0, $E0, $01
        jml $00cad9
HIJACK_00CAFB:
        PHX : TYX : sta.l hdma_table,x : PLX
        db $E8
        jml $00caff
HIJACK_03C609:
        sta.l hdma_table,x
        db $3A
        jml $03c60d
HIJACK_03C612:
        sta.l hdma_table,x
        db $AD, $7C, $14
        jml $03c618
HIJACK_0CAB7C:
        sta.l hdma_table,x
        db $99, $7E, $05
        jml $0cab82































CodeEnd:
	print "pc: ", pc
