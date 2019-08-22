if not pcall(require, "lpeg") then
   io.write[[
You must install the LPEG package to use this assembler.

With LuaRocks installed:

    luarocks install lpeg
]]
   os.exit(1)
end

local declared = {}
function globals(...)
   for _,v in ipairs{...} do
      declared[v] = true
   end
end
local _mt = {}
function _mt:__newindex(k,v)
   if not declared[k] then
      error("Undeclared global: "..tostring(k), 2)
   else
      rawset(self,k,v)
   end
end
_mt.__index = _mt.__newindex
setmetatable(_G, _mt)

if #arg ~= 2 and #arg ~= 3 then
   io.write[[
Usage: bdasm input.s output.txt [listfile.txt]
]]
   os.exit(1)
end

globals("input_filename", "output_filename", "listing_filename")

input_filename = arg[1]
output_filename = arg[2]
listing_filename = arg[3]
