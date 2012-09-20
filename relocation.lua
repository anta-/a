--[[
	アクセスしたPCを表示するだけ
]]

local watchMemorys = {
	{addr = 0x7E14C8, size = 0xC, name = "$14C8"},
	{addr = 0x7E14D4, size = 0xC, name = "RAM_SpriteYHi"},
	{addr = 0x7E14E0, size = 0xC, name = "RAM_SpriteXHi"},
	{addr = 0x7E14EC, size = 0xC, name = "$14EC"},
	{addr = 0x7E14F8, size = 0xC, name = "$14F8"},
	{addr = 0x7E1504, size = 0xC, name = "$1504"},
	{addr = 0x7E1510, size = 0xC, name = "$1510"},
	{addr = 0x7E151C, size = 0xC, name = "$151C"},
	{addr = 0x7E1528, size = 0xC, name = "$1528"},
	{addr = 0x7E1534, size = 0xC, name = "$1534"},
	{addr = 0x7E1540, size = 0xC, name = "$1540"},
	{addr = 0x7E154C, size = 0xC, name = "RAM_DisableInter"},
	{addr = 0x7E1558, size = 0xC, name = "$1558"},
	{addr = 0x7E1564, size = 0xC, name = "$1564"},
	{addr = 0x7E1570, size = 0xC, name = "$1570"},
	{addr = 0x7E157C, size = 0xC, name = "RAM_SpriteDir"},
	{addr = 0x7E1588, size = 0xC, name = "RAM_SprObjStatus"},
	{addr = 0x7E1594, size = 0xC, name = "$1594"},
	{addr = 0x7E15EA, size = 0xC, name = "RAM_OffscreenHorz"},
	{addr = 0x7E15AC, size = 0xC, name = "$15AC"},
	{addr = 0x7E15B8, size = 0xC, name = "$15B8"},
	{addr = 0x7E15C4, size = 0xC, name = "$15C4"},
	{addr = 0x7E15D0, size = 0xC, name = "$15D0"},
	{addr = 0x7E15DC, size = 0xC, name = "$15DC"},
	{addr = 0x7E15EA, size = 0xC, name = "RAM_SprOAMIndex"},
	{addr = 0x7E15F6, size = 0xC, name = "RAM_SpritePal"},
	{addr = 0x7E1602, size = 0xC, name = "$1602"},
	{addr = 0x7E160E, size = 0xC, name = "$160E"},
	{addr = 0x7E161A, size = 0xC, name = "RAM_SprIndexInLvl"},
	{addr = 0x7E1626, size = 0xC, name = "$1626"},
	{addr = 0x7E1632, size = 0xC, name = "RAM_SprBehindScrn"},
	{addr = 0x7E163E, size = 0xC, name = "$163E"},
	{addr = 0x7E164A, size = 0xC, name = "$164A"},
	{addr = 0x7E1656, size = 0xC, name = "RAM_Tweaker1656"},
	{addr = 0x7E1662, size = 0xC, name = "RAM_Tweaker1662"},
	{addr = 0x7E166E, size = 0xC, name = "RAM_Tweaker166E"},
	{addr = 0x7E167A, size = 0xC, name = "RAM_Tweaker167A"},
	{addr = 0x7E1686, size = 0xC, name = "RAM_Tweaker1686"},
	{addr = 0x7E186C, size = 0xC, name = "RAM_OffscreenVert"},
	{addr = 0x7E187B, size = 0xC, name = "$187B"},
	{addr = 0x7E190F, size = 0xC, name = "RAM_Tweaker190F"},
	{addr = 0x7E1FD6, size = 0xC, name = "$1FD6"},
	{addr = 0x7E1FE2, size = 0xC, name = "$1FE2"},
}
local knownPCs = {}

for _, o in ipairs(watchMemorys) do
	memory.registerread(o.addr, o.size, function(addr, size) memorAccess(addr, size, o, false) end)
	memory.registerwrite(o.addr, o.size, function(addr, size) memorAccess(addr, size, o, true) end)
	memory.registerexec(o.addr, o.size, function(addr, size)
--		print(string.format("exec %06X", addr))
	end)
end

function getPC()
	return memory.getregister("pbpc")
end

function memorAccess(addr, size, info, isWrite)
	local pc = getPC()
	if knownPCs[pc] then return end
	knownPCs[pc] = true
	print(string.format("'%s' (%06X = %06X+%X) %s %s access at %06X",
		info.name, addr, info.addr, addr - info.addr,
		isWrite and "write" or "read",
		size == 1 and "byte" or (size == 2 and "word" or size),
		pc))
end
