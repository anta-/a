--[[
	アクセスしたPCを表示するだけ
]]

local watchMemorys = {
	{addr = 0x7E0FBE, size = 0x200, name = "16x pointer"}
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
