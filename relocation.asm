header
lorom

;##############################
; ���ۂ�hijack�������




;##############################
; hijack�p�̋󂫗̈�
!CodeOffset = $108000

!RatsCodeSize = CodeEnd-!CodeOffset-$08
org !CodeOffset
db "STAR"
dw !RatsCodeSize-$01
dw !RatsCodeSize-$01^$FFFF

db "Hello!"

































CodeEnd:
	print "pc: ", pc
