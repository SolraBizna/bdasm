globals("instructions")

instructions = {
   WIDEMOVE={"lvalue", "bigvalue", source=[[
        MOVE A, bhi
        MOVE B, blo
        MOVE a, AB
]]},
   PUSH={"value", source=[[
        MOVE B, S
        MOVE S, BDEC
        MOVE MA, S
        MOVE MD, a
]]},
   WIDEPUSH={"bigvalue", source=[[
        MOVE B, S
        MOVE S, BDEC
        MOVE MA, S
        WIDEMOVE MD, a
]]},
   POP={"lvalue", source=[[
        MOVE MA, S
        MOVE B, S
        MOVE S, BINC
        MOVE a, MD
]]},
   RET={source=[[
        POP PC
]]},
   JL={"bigvalue", source=[[
        WIDEPUSH _A
        WIDEMOVE PC, a
_A:
]]},
   J={"bigvalue", source=[[
        WIDEMOVE PC, a
_A:
]]},
   LOAD={"lvalue", "value", source=[[
        MOVE MA, b
        MOVE a, MD
]]},
   FARLOAD={"lvalue", "bigvalue", source=[[
        WIDEMOVE MA, b
        MOVE a, MD
]]},
   STORE={"value", "value", source=[[
        MOVE MA, a
        MOVE MD, b
]]},
   FARSTORE={"bigvalue", "value", source=[[
        WIDEMOVE MA, a
        MOVE MD, b
]]},
   WIDESTORE={"value", "bigvalue", source=[[
        MOVE MA, a
        WIDEMOVE MD, b
]]},
   FARWIDESTORE={"bigvalue", "bigvalue", source=[[
        WIDEMOVE MA, a
        WIDEMOVE MD, b
]]},
   MOVE={"lvalue", "value", compiled={
            {type="instruction",dest="a", source="b"},
        }},
}

for k,v in pairs(instructions) do v.name = k end

local function remap(x, params)
   if x == "a" then return assert(params[1][2])
   elseif x == "b" then return assert(params[2][2])
   elseif x == "alo" then return assert(params[1][2].."lo")
   elseif x == "blo" then return assert(params[2][2].."lo")
   elseif x == "ahi" then return assert(params[1][2].."hi")
   elseif x == "bhi" then return assert(params[2][2].."hi")
   else return x end
end

local function maybe_map_label(x, labels)
   if x:sub(1,1) ~= "_" then return x end
   local prefix
   if x:sub(-2,-1) == "lo" then prefix,x = "<",x:sub(1,-3)
   elseif x:sub(-2,-1) == "hi" then prefix,x = ">",x:sub(1,-3)
   else prefix = "" end
   assert(labels[x], "unknown label in instruction source")
   return prefix..labels[x]
end

local lpeg = require "lpeg"
local function compile_instruction(insn)
   if insn.compiled then return end
   if insn.compiling then
      error("recursive instruction dependency")
   end
   insn.compiling = true
   assert(insn.source, "attempting to compile an instruction with no source")
   local compiled = {}
   local labels = {}
   for l in insn.source:gmatch("[^\n]+") do
      local label, ltyp, name, condition, update, params = lpeg.match(grammar, l)
      if not label then
         error("unparseable source line compiling instruction "..insn.name)
      elseif label ~= "" then
         assert(label:sub(1,1) == "_", "instruction source labels must begin with _")
         labels[label] = "+"..#compiled
      end
      if ltyp ~= nil then
         if ltyp == "directive" then
            error("directive in source for instruction "..insn.name)
         elseif condition ~= "" or update ~= false then
            error("explicit condition flag / update in source for instruction "..insn.name)
         elseif not instructions[name] then
            error("referenced nonexistent instruction "..name.." in source for instruction "..insn.name)
         end
         compile_instruction(instructions[name])
         local subcode = instructions[name].compiled
         for n=1,#subcode do
            local subel = subcode[n]
            assert(subel.type == "instruction")
            local neo = {type="instruction"}
            neo.source = remap(subel.source, params)
            neo.dest = remap(subel.dest, params)
            compiled[#compiled+1] = neo
         end
      end
   end
   for n=1,#compiled do
      local el = compiled[n]
      el.source = maybe_map_label(el.source, labels)
      el.dest = maybe_map_label(el.dest, labels)
   end
   insn.compiling = nil
   insn.compiled = compiled
end

for k,v in pairs(instructions) do compile_instruction(v) end

-- uncomment to debug instruction compilation
--[=[
globals("unpack")
unpack=table.unpack
local t = {}
for k,v in pairs(instructions) do t[#t+1] = k end
table.sort(t)
for n=1,#t do
   local insn = instructions[t[n]]
   print(insn.name..":")
   for n=1,#insn.compiled do
      print(require "erebor".build(insn.compiled[n]))
   end
end
os.exit(1)
--[=[]=]
