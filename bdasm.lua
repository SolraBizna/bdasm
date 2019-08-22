#!/usr/bin/env lua5.3

-- This file is concatentated from several smaller files, to allow the
-- program's logic to be spread out without complicating its invocation
-- and/or distribution processes.

load("if not pcall(require, \"lpeg\") then\
   io.write[[\
You must install the LPEG package to use this assembler.\
\
With LuaRocks installed:\
\
    luarocks install lpeg\
]]\
   os.exit(1)\
end\
\
local declared = {}\
function globals(...)\
   for _,v in ipairs{...} do\
      declared[v] = true\
   end\
end\
local _mt = {}\
function _mt:__newindex(k,v)\
   if not declared[k] then\
      error(\"Undeclared global: \"..tostring(k), 2)\
   else\
      rawset(self,k,v)\
   end\
end\
_mt.__index = _mt.__newindex\
setmetatable(_G, _mt)\
\
if #arg ~= 2 and #arg ~= 3 then\
   io.write[[\
Usage: bdasm input.s output.txt [listfile.txt]\
]]\
   os.exit(1)\
end\
\
globals(\"input_filename\", \"output_filename\", \"listing_filename\")\
\
input_filename = arg[1]\
output_filename = arg[2]\
listing_filename = arg[3]\
","@src/top.lua")()
load("globals(\"grammar\")\
\
local lpeg = require \"lpeg\"\
\
local P = lpeg.P\
local V = lpeg.V\
local C = lpeg.C\
local R = lpeg.R\
local S = lpeg.S\
local Ct = lpeg.Ct\
local Cc = lpeg.Cc\
\
local ESCAPES = {\
   [\"\\\\\"] = \"\\\\\",\
   a=\"\\x07\",\
   n=\"\\n\",\
   t=\"\\t\",\
   r=\"\\r\",\
}\
\
grammar = P{\
   \"loc\";\
   loc = V\"label\" * (V\"instruction\" + V\"eol\");\
   label = (-1 * Cc\"\") + (C(V\"identifier\") * (P\":\")^-1 + Cc\"\") * (V\"whitespace\"^1+V\"eol\");\
   eol = V\"whitespace\"^0 * (P\";\" + -1);\
   identifier = (R(\"az\",\"AZ\")+S\"_\") * (R(\"az\",\"AZ\",\"09\")+S\"_\")^0 + P\"-\"^1 + P\"+\"^1;\
   instruction = (Cc\"instruction\"*(C(V\"identifier\")/string.upper)*((P\".\"*C(V\"identifier\")/string.upper)+C\"\")*(P\"?\"*Cc(true)+Cc(false))+(Cc\"directive\"*P\".\"*(C(V\"identifier\")/string.upper)*Cc\"\"*Cc(false))) * Ct(V\"whitespace\"^1 * V\"nonterminal_parameter\"^0 * V\"parameter\" + V\"eol\");\
   nonterminal_parameter = V\"parameter\" * V\"whitespace\"^0 * P\",\" * V\"whitespace\"^0;\
   parameter = V\"param_string\" + V\"param_id\" + V\"param_literal\";\
   param_string = V\"squot_string\" + V\"dquot_string\";\
   squot_string = Ct(Cc\"string\"*P\"'\"*C((P(1)-P\"'\")^0)*\"'\");\
   dquot_string = Ct(Cc\"string\"*P'\"'*(Ct((V\"dquot_string_char\"-P'\"')^0)/table.concat)*P'\"');\
   dquot_string_char = V\"dquot_escaped_char\" + C(1);\
   dquot_escaped_char = P\"\\\\\" * (C(S\"\\\\antr\") / ESCAPES);\
   param_id = Ct(Cc\"identifier\"*C(V\"identifier\"));\
   param_literal = Ct(Cc\"integer\"*(V\"num_hex\" + V\"num_bin\" + V\"num_dec\"));\
   num_hex = P\"0x\" * C(R(\"09\",\"AF\",\"af\")^1)/function(x) return tonumber(x,16) end;\
   num_bin = P\"%\" * C(S\"01\"^1)/function(x) return tonumber(x,2) end;\
   num_dec = C(P\"-\"^-1*R(\"09\")^1)/function(x) return tonumber(x,10) end;\
   whitespace = S\" \\r\\t\";\
}\
","@src/grammar.lua")()
load("globals(\"source_registers\", \"dest_registers\", \"all_registers\", \"register_names\", \"conditions\", \"reverse_condition_map\")\
\
local common_registers = {\
   A=1,\
   B=2,\
   PC=3,\
   MA=4,\
   MD=5,\
   S=6,\
   X=7\
}\
\
source_registers = {\
   ADD=0<<3,\
   SUB=1<<3,\
   LSL=2<<3,\
   LSR=3<<3,\
   ROL=4<<3,\
   ROR=5<<3,\
   AB=6<<3,\
   AINC=7<<3,\
   BINC=8<<3,\
   ADEC=9<<3,\
   BDEC=10<<3,\
   ASEX=11<<3,\
   NOT=12<<3,\
   AND=13<<3,\
   OR=14<<3,\
   XOR=15<<3,\
}\
\
dest_registers = {\
   Q=0,\
}\
\
for k,v in pairs(common_registers) do\
   source_registers[k] = v\
   dest_registers[k] = v\
end\
\
all_registers = {}\
register_names = {}\
\
for k,v in pairs(source_registers) do\
   register_names[v] = k\
   all_registers[k] = v\
end\
\
for k,v in pairs(dest_registers) do\
   register_names[v] = k\
   all_registers[k] = v\
end\
\
conditions = {\
   AL=0,\
   NEVER=1,\
   NAL=1,\
   MI=2,\
   NMI=3,\
   PL=4,\
   NPL=5,\
   Z=6,\
   NZ=7\
}\
\
reverse_condition_map = {\
   [0] = \"\",\
   [1] = \".NEVER\",\
   [2] = \".MI\",\
   [3] = \".NMI\",\
   [4] = \".PL\",\
   [5] = \".NPL\",\
   [6] = \".Z\",\
   [7] = \".NZ\",\
}\
","@src/registers.lua")()
load("globals(\"instructions\")\
\
instructions = {\
   WIDEMOVE={\"lvalue\", \"bigvalue\", source=[[\
        MOVE A, bhi\
        MOVE B, blo\
        MOVE a, AB\
]]},\
   PUSH={\"value\", source=[[\
        MOVE B, S\
        MOVE S, BDEC\
        MOVE MA, S\
        MOVE MD, a\
]]},\
   WIDEPUSH={\"bigvalue\", source=[[\
        MOVE B, S\
        MOVE S, BDEC\
        MOVE MA, S\
        WIDEMOVE MD, a\
]]},\
   POP={\"lvalue\", source=[[\
        MOVE MA, S\
        MOVE B, S\
        MOVE S, BINC\
        MOVE a, MD\
]]},\
   RET={source=[[\
        POP PC\
]]},\
   JL={\"bigvalue\", source=[[\
        WIDEPUSH _A\
        WIDEMOVE PC, a\
_A:\
]]},\
   J={\"bigvalue\", source=[[\
        WIDEMOVE PC, a\
_A:\
]]},\
   LOAD={\"lvalue\", \"value\", source=[[\
        MOVE MA, b\
        MOVE a, MD\
]]},\
   FARLOAD={\"lvalue\", \"bigvalue\", source=[[\
        WIDEMOVE MA, b\
        MOVE a, MD\
]]},\
   STORE={\"value\", \"value\", source=[[\
        MOVE MA, a\
        MOVE MD, b\
]]},\
   FARSTORE={\"bigvalue\", \"value\", source=[[\
        WIDEMOVE MA, a\
        MOVE MD, b\
]]},\
   WIDESTORE={\"value\", \"bigvalue\", source=[[\
        MOVE MA, a\
        WIDEMOVE MD, b\
]]},\
   FARWIDESTORE={\"bigvalue\", \"bigvalue\", source=[[\
        WIDEMOVE MA, a\
        WIDEMOVE MD, b\
]]},\
   MOVE={\"lvalue\", \"value\", compiled={\
            {type=\"instruction\",dest=\"a\", source=\"b\"},\
        }},\
}\
\
for k,v in pairs(instructions) do v.name = k end\
\
local function remap(x, params)\
   if x == \"a\" then return assert(params[1][2])\
   elseif x == \"b\" then return assert(params[2][2])\
   elseif x == \"alo\" then return assert(params[1][2]..\"lo\")\
   elseif x == \"blo\" then return assert(params[2][2]..\"lo\")\
   elseif x == \"ahi\" then return assert(params[1][2]..\"hi\")\
   elseif x == \"bhi\" then return assert(params[2][2]..\"hi\")\
   else return x end\
end\
\
local function maybe_map_label(x, labels)\
   if x:sub(1,1) ~= \"_\" then return x end\
   local prefix\
   if x:sub(-2,-1) == \"lo\" then prefix,x = \"<\",x:sub(1,-3)\
   elseif x:sub(-2,-1) == \"hi\" then prefix,x = \">\",x:sub(1,-3)\
   else prefix = \"\" end\
   assert(labels[x], \"unknown label in instruction source\")\
   return prefix..labels[x]\
end\
\
local lpeg = require \"lpeg\"\
local function compile_instruction(insn)\
   if insn.compiled then return end\
   if insn.compiling then\
      error(\"recursive instruction dependency\")\
   end\
   insn.compiling = true\
   assert(insn.source, \"attempting to compile an instruction with no source\")\
   local compiled = {}\
   local labels = {}\
   for l in insn.source:gmatch(\"[^\\n]+\") do\
      local label, ltyp, name, condition, update, params = lpeg.match(grammar, l)\
      if not label then\
         error(\"unparseable source line compiling instruction \"..insn.name)\
      elseif label ~= \"\" then\
         assert(label:sub(1,1) == \"_\", \"instruction source labels must begin with _\")\
         labels[label] = \"+\"..#compiled\
      end\
      if ltyp ~= nil then\
         if ltyp == \"directive\" then\
            error(\"directive in source for instruction \"..insn.name)\
         elseif condition ~= \"\" or update ~= false then\
            error(\"explicit condition flag / update in source for instruction \"..insn.name)\
         elseif not instructions[name] then\
            error(\"referenced nonexistent instruction \"..name..\" in source for instruction \"..insn.name)\
         end\
         compile_instruction(instructions[name])\
         local subcode = instructions[name].compiled\
         for n=1,#subcode do\
            local subel = subcode[n]\
            assert(subel.type == \"instruction\")\
            local neo = {type=\"instruction\"}\
            neo.source = remap(subel.source, params)\
            neo.dest = remap(subel.dest, params)\
            compiled[#compiled+1] = neo\
         end\
      end\
   end\
   for n=1,#compiled do\
      local el = compiled[n]\
      el.source = maybe_map_label(el.source, labels)\
      el.dest = maybe_map_label(el.dest, labels)\
   end\
   insn.compiling = nil\
   insn.compiled = compiled\
end\
\
for k,v in pairs(instructions) do compile_instruction(v) end\
\
-- uncomment to debug instruction compilation\
--[=[\
globals(\"unpack\")\
unpack=table.unpack\
local t = {}\
for k,v in pairs(instructions) do t[#t+1] = k end\
table.sort(t)\
for n=1,#t do\
   local insn = instructions[t[n]]\
   print(insn.name..\":\")\
   for n=1,#insn.compiled do\
      print(require \"erebor\".build(insn.compiled[n]))\
   end\
end\
os.exit(1)\
--[=[]=]\
","@src/instructions.lua")()
load("globals(\"memory\", \"mem_base\", \"mem_size\")\
\
local input,e = io.open(input_filename, \"rb\")\
if not input then\
   print(e)\
   os.exit(1)\
end\
\
local listing\
if listing_filename then\
   listing,e = io.open(listing_filename, \"wb\")\
   if not listing then\
      print(e)\
      os.exit(1)\
   end\
end\
\
local lpeg = require \"lpeg\"\
\
local cur_line\
local lineno = 1\
local printed_line = nil\
local error_count = 0\
local function asm_error(format, ...)\
   io.write(\"ERROR: \",input_filename,\":\",lineno,\": \",format:format(...),\"\\n\")\
   if printed_line ~= lineno and cur_line then\
      io.write(\" \",cur_line,\"\\n\")\
      printed_line = lineno\
   end\
   error_count = error_count + 1\
end\
local had_fatal_error = false -- stop parsing if true\
local function fatal_asm_error(...)\
   asm_error(...)\
   fatal_error = true\
end\
\
memory = {}\
local definitions = {}\
local had_code = false -- .BASE and .SIZE are invalid when had_code is true\
mem_base = 0x8000\
mem_size = 0x8000\
local instruction_start_org\
local org = 0x8000\
local refs = {}\
\
local function resolve(name)\
   local got\
   local rtype, offset = name:match(\"^([<>])%+([0-9]+)$\")\
   if rtype then\
      -- offset relative to start of instruction\
      got = instruction_start_org + tonumber(offset, 10)\
      if rtype == \"<\" then got = got & 255\
      elseif rtype == \">\" then got = (got >> 8) & 255\
      end\
   elseif name:sub(1,1) == \"<\" then\
      -- low part\
      got = definitions[name:sub(2,-1)]\
      if got then got = got & 255 end\
   elseif name:sub(1,1) == \">\" then\
      -- high part\
      got = definitions[name:sub(2,-1)]\
      if got then got = (got >> 8) & 255 end\
   elseif name:sub(1,1) == \"!\" then\
      -- full word\
      got = definitions[name:sub(2,-1)]\
   else\
      -- word fitting in a uint8\
      got = definitions[name]\
      if got and (got < 0 or got > 255) then\
         got = 0\
         asm_error(\"definition of %s does not fit in a uint8\", name)\
      end\
   end\
   return got\
end\
\
local function warn_listing_seek()\
   print(\"WARNING: Couldn't seek in the listing file. Some references will be missing.\")\
   warn_listing_seek = function() end\
end\
\
local function link(ref, got)\
   memory[ref.addr] = memory[ref.addr] | got\
   if listing and ref.lspos then\
      if not listing:seek(\"set\", ref.lspos) then\
         warn_listing_seek()\
      else\
         listing:write((\"%04X\"):format(memory[ref.addr]))\
         assert(listing:seek(\"end\"))\
      end\
   end\
end\
\
local function ref(addr, name)\
   local lspos\
   if listing then\
      lspos = listing:seek()\
      if lspos then lspos = lspos + 10\
      else warn_listing_seek() end\
   end\
   refs[#refs+1] = {addr=addr, name=name, lineno=lineno, lspos=lspos}\
end\
\
local function emit_word(w, ex)\
   if org < mem_base or org >= mem_base + mem_size then\
      asm_error(\".ORG value has escaped from memory\")\
   end\
   had_code = true\
   if memory[org] then\
      asm_error(\"attempted to emit code in an already-occupied section of the image\")\
   else\
      memory[org] = w\
   end\
   if listing then\
      listing:write((\";;0x%04X: %04X%s\\n\"):format(org, w, ex or \"\"))\
   end\
   org = org + 1\
end\
\
local function emit_move(source, dest, condition, update)\
   local word = dest << 9\
   local sourcename\
   if type(source) == \"number\" then\
      word = word | source\
      if source >= 256 then sourcename = source&255\
      else sourcename = register_names[source] end\
   else\
      sourcename = source\
      local got = resolve(source)\
      if not got then\
         ref(org, source)\
         word = word | 256\
      else\
         source = got\
         word = word | got | 256\
      end\
   end\
   if update then word = word | 0x1000 end\
   word = word | (condition << 13)\
   local ex = (\" = MOVE%s%s %s, %s\"):format(reverse_condition_map[condition], update and \"?\" or \"\", register_names[dest], sourcename)\
   emit_word(word, ex)\
end\
\
local function ck16(x)\
   if type(x) ~= \"number\" or x < -32768 or x > 65535 then\
      asm_error(\"expected an integer between -32768 and 65535\")\
      return false\
   else\
      return true\
   end\
end\
\
local function cku16(x)\
   if type(x) ~= \"number\" or x < 0 or x > 65535 then\
      asm_error(\"expected an integer between 0 and 65535\")\
      return false\
   else\
      return true\
   end\
end\
\
local directives = {}\
directives.BASE = {\"integer\",\
func=function(a)\
   if had_code then\
      return fatal_asm_error(\".BASE is only valid before code\")\
   end\
   if not cku16(a) then return end\
   mem_base = a\
end}\
directives.SIZE = {\"integer\",\
func=function(a)\
   if had_code then\
      return fatal_asm_error(\".SIZE is only valid before code\")\
   end\
   if not cku16(a) then return end\
   mem_size = a\
end}\
directives.ORG = {\"integer\",\
func=function(a)\
   if not cku16(a) then return end\
   org = a\
   if org < mem_base or org >= mem_base + mem_size then\
      asm_error(\".ORG value must be in the range 0x%04X - 0x%04X\",\
                mem_base, mem_base + mem_size - 1)\
   end\
end}\
directives.DEFINE = {\"identifier\", \"integer\",\
func=function(id,val)\
   if all_registers[id] then\
      asm_error(\"\\\"%s\\\" is a register name and cannot be used as a symbol\", id)\
   elseif definitions[id] then\
      asm_error(\"Duplicate definition for \\\"%s\\\"\", id)\
   elseif not ck16(val) then\
      return\
   else\
      definitions[id] = val\
   end\
end}\
local byte_exes = {[10]=\" = newline\",[13]=\" = carriage return\",[7]=\" = bell\",[9]=\" = tab\"}\
directives.WORDS = {\
func=function(params)\
   if #params == 0 then\
      asm_error(\".WORDS expects at least one parameter\")\
      return\
   end\
   for n=1,#params do\
      local param = params[n]\
      local typ,val = param[1], param[2]\
      if typ == \"integer\" then\
         if ck16(val) then emit_word(val, (\" = %i\"):format(val)) end\
      elseif typ == \"identifier\" then\
         asm_error(\"Can't emit linked words yet.\")\
      elseif typ == \"string\" then\
         for n=1,#val do\
            local byte = val:byte(n)\
            local ex\
            if byte_exes[byte] then ex = byte_exes[byte]\
            elseif byte >= 32 and byte <= 126 then\
               ex = (\" = '%s'\"):format(string.char(byte))\
            else\
               ex = (\" = %i\"):format(byte)\
            end\
            emit_word(byte, ex)\
         end\
      else\
         error(\"Internal error: Unknown parameter type: \"..typ)\
      end\
   end\
end}\
\
local function remap(name, params)\
   if params[name] then return params[name] end\
   return name\
end\
for l in input:lines() do\
   cur_line = l\
   if listing then\
      listing:write(l, \"\\n\")\
   end\
   local label, ltype, name, condition, update, params = lpeg.match(grammar, l)\
   if not label then\
      asm_error(\"Couldn't parse this line.\")\
   elseif label ~= \"\" then\
      if all_registers[label] then\
         asm_error(\"\\\"%s\\\" is a register name and cannot be used as a symbol\", id)\
      elseif definitions[label] and label:sub(1,1) ~= \"-\" then\
         asm_error(\"Duplicate definition for \\\"%s\\\"\", label)\
      elseif label:sub(1,1) == \"+\" then\
         -- \"forward jump\" label, fixup backwards\
         local lo = \"<\"..label\
         local hi = \">\"..label\
         for n=#refs,1,-1 do\
            if refs[n].name == lo then\
               link(refs[n], org & 255)\
               table.remove(refs, n)\
            elseif refs[n].name == hi then\
               link(refs[n], (org >> 8) & 255)\
               table.remove(refs, n)\
            end\
         end\
      else\
         -- regular label or \"backward jump\" label\
         definitions[label] = org\
      end\
   end\
   if ltype == \"directive\" then\
      local directive = directives[name]\
      if not directive then\
         asm_error(\"Unknown directive: \\\".%s\\\"\", name)\
      elseif #directive == 0 then\
         -- varargs directive, does its own validation\
         directive.func(params)\
      else\
         if #directive ~= #params then\
            asm_error(\"\\\".%s\\\" directive expects %i parameter%s\",\
                      name, #directive, #directive == 1 and \"\" or \"s\")\
         else\
            local inparams = {}\
            for n=1, #params do\
               if params[n][1] ~= directive[n] then\
                  asm_error(\"\\\".%s\\\" directive parameter %i must be %s, not %s\",\
                            name, n, directive[n], params[n][1])\
                  inparams = nil\
               elseif inparams then\
                  inparams[n] = params[n][2]\
               end\
            end\
            if inparams ~= nil then\
               directive.func(table.unpack(inparams))\
            end\
         end\
      end\
   elseif ltype == \"instruction\" then\
      instruction_start_org = org\
      local instruction = instructions[name]\
      if not instruction then\
         asm_error(\"Unknown instruction: \\\"%s\\\"\", name)\
      elseif #instruction ~= #params then\
         asm_error(\"\\\"%s\\\" instruction expects %i parameter%s\",\
                   name, #instruction, #instruction == 1 and \"\" or \"s\")\
      else\
         if condition == \"\" then\
            condition = 0 -- default is always\
         elseif not conditions[condition] then\
            asm_error(\"invalid condition\")\
            condition = 0\
         else\
            condition = conditions[condition]\
         end\
         local ok = true\
         local source_params = {}\
         for n=1,#instruction do\
            if params[n][1] == \"string\" then\
               asm_error(\"instruction parameters may not be strings\")\
               ok = false\
            else\
               if params[n][1] == \"integer\" then\
                  if not ck16(params[n][2]) then ok = false end\
                  params[n][2] = params[n][2] & 65535\
               end\
               local name = string.char(96+n)\
               if instruction[n] == \"bigvalue\" then\
                  if params[n][1] == \"integer\" then\
                     source_params[name..\"lo\"] = params[n][2]&255\
                     source_params[name..\"hi\"] = (params[n][2]>>8)&255\
                  else\
                     source_params[name..\"lo\"] = \"<\"..params[n][2]\
                     source_params[name..\"hi\"] = \">\"..params[n][2]\
                  end\
               elseif instruction[n] == \"lvalue\" then\
                  if params[n][1] ~= \"identifier\" then\
                     asm_error(\"parameter %i of %s must be a register\",\
                               n, instruction.name)\
                     ok = false\
                  else\
                     -- TODO: check that it is indeed an lvalue\
                  end\
                  source_params[name] = params[n][2]\
               elseif instruction[n] == \"value\" then\
                  source_params[name] = params[n][2]\
               else\
                  error(\"internal error: unknown parameter type: \"..instruction[n])\
               end\
            end\
         end\
         if ok then\
            for n=1,#instruction.compiled do\
               local source, dest\
               local el = instruction.compiled[n]\
               local we_update = update and n == #instruction.compiled\
               dest = remap(el.dest, source_params)\
               if dest_registers[dest] then\
                  dest = dest_registers[dest]\
               else\
                  asm_error(\"not a valid destination register: %s\", dest)\
                  dest = nil\
               end\
               source = remap(el.source, source_params)\
               if type(source) == \"number\" then\
                  if source > 255 or source < 0 then\
                     asm_error(\"source is not a valid uint8\")\
                  end\
                  source = source & 255 | 256\
               elseif source_registers[source] then\
                  source = source_registers[source]\
               end\
               if source and dest then\
                  emit_move(source, dest, condition, we_update)\
               else\
                  break\
               end\
            end\
         end\
      end\
   elseif ltype == nil then\
      -- (no line, or couldn't parse)\
   else\
      error(\"Internal error: unknown ltype: \"..ltype)\
   end\
   lineno = lineno + 1\
   if error_count > 50 then\
      print(\"Too many errors, giving up...\")\
      break\
   elseif had_fatal_error then\
      print(\"That error is fatal...\")\
      break\
   end\
end\
input:close()\
\
if error_count > 0 then\
   if error_count == 1 then\
      print(\"Encountered an error in the assembly process. Aborting.\")\
   else\
      print(\"Encountered \"..error_count..\" errors in the assembly process. Aborting.\")\
   end\
   os.exit(1)\
end\
\
cur_line = nil\
\
for n=1,#refs do\
   local ref = refs[n]\
   lineno = ref.lineno\
   local got = resolve(ref.name)\
   if not got then\
      asm_error(\"undefined reference to \\\"%s\\\"\", ref.name)\
   else\
      link(ref, got)\
   end\
end\
\
if error_count > 0 then\
   if error_count == 1 then\
      print(\"Encountered an error in the linking process. Aborting.\")\
   else\
      print(\"Encountered \"..error_count..\" errors in the linking process. Aborting.\")\
   end\
   os.exit(1)\
end\
\
if listing then listing:close() end\
","@src/assemble.lua")()
load("local output, e = io.open(output_filename, \"wb\")\
if not output then\
   print(e)\
   os.exit(1)\
end\
\
output:write(\"v2.0 raw\\n\\n\")\
\
local highest_addr = mem_base + mem_size - 1\
while (memory[highest_addr] == 0 or memory[highest_addr] == nil) and highest_addr >= mem_base do\
   highest_addr = highest_addr - 1\
end\
\
for n=mem_base, highest_addr do\
   output:write((\"%04X\\n\"):format(memory[n]))\
end\
output:close()\
","@src/output.lua")()
