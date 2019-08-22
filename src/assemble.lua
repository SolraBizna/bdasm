globals("memory", "mem_base", "mem_size")

local input,e = io.open(input_filename, "rb")
if not input then
   print(e)
   os.exit(1)
end

local listing
if listing_filename then
   listing,e = io.open(listing_filename, "wb")
   if not listing then
      print(e)
      os.exit(1)
   end
end

local lpeg = require "lpeg"

local cur_line
local lineno = 1
local printed_line = nil
local error_count = 0
local function asm_error(format, ...)
   io.write("ERROR: ",input_filename,":",lineno,": ",format:format(...),"\n")
   if printed_line ~= lineno and cur_line then
      io.write(" ",cur_line,"\n")
      printed_line = lineno
   end
   error_count = error_count + 1
end
local had_fatal_error = false -- stop parsing if true
local function fatal_asm_error(...)
   asm_error(...)
   fatal_error = true
end

memory = {}
local definitions = {}
local had_code = false -- .BASE and .SIZE are invalid when had_code is true
mem_base = 0x8000
mem_size = 0x8000
local instruction_start_org
local org = 0x8000
local refs = {}

local function resolve(name)
   local got
   local rtype, offset = name:match("^([<>])%+([0-9]+)$")
   if rtype then
      -- offset relative to start of instruction
      got = instruction_start_org + tonumber(offset, 10)
      if rtype == "<" then got = got & 255
      elseif rtype == ">" then got = (got >> 8) & 255
      end
   elseif name:sub(1,1) == "<" then
      -- low part
      got = definitions[name:sub(2,-1)]
      if got then got = got & 255 end
   elseif name:sub(1,1) == ">" then
      -- high part
      got = definitions[name:sub(2,-1)]
      if got then got = (got >> 8) & 255 end
   elseif name:sub(1,1) == "!" then
      -- full word
      got = definitions[name:sub(2,-1)]
   else
      -- word fitting in a uint8
      got = definitions[name]
      if got and (got < 0 or got > 255) then
         got = 0
         asm_error("definition of %s does not fit in a uint8", name)
      end
   end
   return got
end

local function warn_listing_seek()
   print("WARNING: Couldn't seek in the listing file. Some references will be missing.")
   warn_listing_seek = function() end
end

local function link(ref, got)
   memory[ref.addr] = memory[ref.addr] | got
   if listing and ref.lspos then
      if not listing:seek("set", ref.lspos) then
         warn_listing_seek()
      else
         listing:write(("%04X"):format(memory[ref.addr]))
         assert(listing:seek("end"))
      end
   end
end

local function ref(addr, name)
   local lspos
   if listing then
      lspos = listing:seek()
      if lspos then lspos = lspos + 10
      else warn_listing_seek() end
   end
   refs[#refs+1] = {addr=addr, name=name, lineno=lineno, lspos=lspos}
end

local function emit_word(w, ex)
   if org < mem_base or org >= mem_base + mem_size then
      asm_error(".ORG value has escaped from memory")
   end
   had_code = true
   if memory[org] then
      asm_error("attempted to emit code in an already-occupied section of the image")
   else
      memory[org] = w
   end
   if listing then
      listing:write((";;0x%04X: %04X%s\n"):format(org, w, ex or ""))
   end
   org = org + 1
end

local function emit_move(source, dest, condition, update)
   local word = dest << 9
   local sourcename
   if type(source) == "number" then
      word = word | source
      if source >= 256 then sourcename = source&255
      else sourcename = register_names[source] end
   else
      sourcename = source
      local got = resolve(source)
      if not got then
         ref(org, source)
         word = word | 256
      else
         source = got
         word = word | got | 256
      end
   end
   if update then word = word | 0x1000 end
   word = word | (condition << 13)
   local ex = (" = MOVE%s%s %s, %s"):format(reverse_condition_map[condition], update and "?" or "", register_names[dest], sourcename)
   emit_word(word, ex)
end

local function ck16(x)
   if type(x) ~= "number" or x < -32768 or x > 65535 then
      asm_error("expected an integer between -32768 and 65535")
      return false
   else
      return true
   end
end

local function cku16(x)
   if type(x) ~= "number" or x < 0 or x > 65535 then
      asm_error("expected an integer between 0 and 65535")
      return false
   else
      return true
   end
end

local directives = {}
directives.BASE = {"integer",
func=function(a)
   if had_code then
      return fatal_asm_error(".BASE is only valid before code")
   end
   if not cku16(a) then return end
   mem_base = a
end}
directives.SIZE = {"integer",
func=function(a)
   if had_code then
      return fatal_asm_error(".SIZE is only valid before code")
   end
   if not cku16(a) then return end
   mem_size = a
end}
directives.ORG = {"integer",
func=function(a)
   if not cku16(a) then return end
   org = a
   if org < mem_base or org >= mem_base + mem_size then
      asm_error(".ORG value must be in the range 0x%04X - 0x%04X",
                mem_base, mem_base + mem_size - 1)
   end
end}
directives.DEFINE = {"identifier", "integer",
func=function(id,val)
   if all_registers[id] then
      asm_error("\"%s\" is a register name and cannot be used as a symbol", id)
   elseif definitions[id] then
      asm_error("Duplicate definition for \"%s\"", id)
   elseif not ck16(val) then
      return
   else
      definitions[id] = val
   end
end}
local byte_exes = {[10]=" = newline",[13]=" = carriage return",[7]=" = bell",[9]=" = tab"}
directives.WORDS = {
func=function(params)
   if #params == 0 then
      asm_error(".WORDS expects at least one parameter")
      return
   end
   for n=1,#params do
      local param = params[n]
      local typ,val = param[1], param[2]
      if typ == "integer" then
         if ck16(val) then emit_word(val, (" = %i"):format(val)) end
      elseif typ == "identifier" then
         asm_error("Can't emit linked words yet.")
      elseif typ == "string" then
         for n=1,#val do
            local byte = val:byte(n)
            local ex
            if byte_exes[byte] then ex = byte_exes[byte]
            elseif byte >= 32 and byte <= 126 then
               ex = (" = '%s'"):format(string.char(byte))
            else
               ex = (" = %i"):format(byte)
            end
            emit_word(byte, ex)
         end
      else
         error("Internal error: Unknown parameter type: "..typ)
      end
   end
end}

local function remap(name, params)
   if params[name] then return params[name] end
   return name
end
for l in input:lines() do
   cur_line = l
   if listing then
      listing:write(l, "\n")
   end
   local label, ltype, name, condition, update, params = lpeg.match(grammar, l)
   if not label then
      asm_error("Couldn't parse this line.")
   elseif label ~= "" then
      if all_registers[label] then
         asm_error("\"%s\" is a register name and cannot be used as a symbol", id)
      elseif definitions[label] and label:sub(1,1) ~= "-" then
         asm_error("Duplicate definition for \"%s\"", label)
      elseif label:sub(1,1) == "+" then
         -- "forward jump" label, fixup backwards
         local lo = "<"..label
         local hi = ">"..label
         for n=#refs,1,-1 do
            if refs[n].name == lo then
               link(refs[n], org & 255)
               table.remove(refs, n)
            elseif refs[n].name == hi then
               link(refs[n], (org >> 8) & 255)
               table.remove(refs, n)
            end
         end
      else
         -- regular label or "backward jump" label
         definitions[label] = org
      end
   end
   if ltype == "directive" then
      local directive = directives[name]
      if not directive then
         asm_error("Unknown directive: \".%s\"", name)
      elseif #directive == 0 then
         -- varargs directive, does its own validation
         directive.func(params)
      else
         if #directive ~= #params then
            asm_error("\".%s\" directive expects %i parameter%s",
                      name, #directive, #directive == 1 and "" or "s")
         else
            local inparams = {}
            for n=1, #params do
               if params[n][1] ~= directive[n] then
                  asm_error("\".%s\" directive parameter %i must be %s, not %s",
                            name, n, directive[n], params[n][1])
                  inparams = nil
               elseif inparams then
                  inparams[n] = params[n][2]
               end
            end
            if inparams ~= nil then
               directive.func(table.unpack(inparams))
            end
         end
      end
   elseif ltype == "instruction" then
      instruction_start_org = org
      local instruction = instructions[name]
      if not instruction then
         asm_error("Unknown instruction: \"%s\"", name)
      elseif #instruction ~= #params then
         asm_error("\"%s\" instruction expects %i parameter%s",
                   name, #instruction, #instruction == 1 and "" or "s")
      else
         if condition == "" then
            condition = 0 -- default is always
         elseif not conditions[condition] then
            asm_error("invalid condition")
            condition = 0
         else
            condition = conditions[condition]
         end
         local ok = true
         local source_params = {}
         for n=1,#instruction do
            if params[n][1] == "string" then
               asm_error("instruction parameters may not be strings")
               ok = false
            else
               if params[n][1] == "integer" then
                  if not ck16(params[n][2]) then ok = false end
                  params[n][2] = params[n][2] & 65535
               end
               local name = string.char(96+n)
               if instruction[n] == "bigvalue" then
                  if params[n][1] == "integer" then
                     source_params[name.."lo"] = params[n][2]&255
                     source_params[name.."hi"] = (params[n][2]>>8)&255
                  else
                     source_params[name.."lo"] = "<"..params[n][2]
                     source_params[name.."hi"] = ">"..params[n][2]
                  end
               elseif instruction[n] == "lvalue" then
                  if params[n][1] ~= "identifier" then
                     asm_error("parameter %i of %s must be a register",
                               n, instruction.name)
                     ok = false
                  else
                     -- TODO: check that it is indeed an lvalue
                  end
                  source_params[name] = params[n][2]
               elseif instruction[n] == "value" then
                  source_params[name] = params[n][2]
               else
                  error("internal error: unknown parameter type: "..instruction[n])
               end
            end
         end
         if ok then
            for n=1,#instruction.compiled do
               local source, dest
               local el = instruction.compiled[n]
               local we_update = update and n == #instruction.compiled
               dest = remap(el.dest, source_params)
               if dest_registers[dest] then
                  dest = dest_registers[dest]
               else
                  asm_error("not a valid destination register: %s", dest)
                  dest = nil
               end
               source = remap(el.source, source_params)
               if type(source) == "number" then
                  if source > 255 or source < 0 then
                     asm_error("source is not a valid uint8")
                  end
                  source = source & 255 | 256
               elseif source_registers[source] then
                  source = source_registers[source]
               end
               if source and dest then
                  emit_move(source, dest, condition, we_update)
               else
                  break
               end
            end
         end
      end
   elseif ltype == nil then
      -- (no line, or couldn't parse)
   else
      error("Internal error: unknown ltype: "..ltype)
   end
   lineno = lineno + 1
   if error_count > 50 then
      print("Too many errors, giving up...")
      break
   elseif had_fatal_error then
      print("That error is fatal...")
      break
   end
end
input:close()

if error_count > 0 then
   if error_count == 1 then
      print("Encountered an error in the assembly process. Aborting.")
   else
      print("Encountered "..error_count.." errors in the assembly process. Aborting.")
   end
   os.exit(1)
end

cur_line = nil

for n=1,#refs do
   local ref = refs[n]
   lineno = ref.lineno
   local got = resolve(ref.name)
   if not got then
      asm_error("undefined reference to \"%s\"", ref.name)
   else
      link(ref, got)
   end
end

if error_count > 0 then
   if error_count == 1 then
      print("Encountered an error in the linking process. Aborting.")
   else
      print("Encountered "..error_count.." errors in the linking process. Aborting.")
   end
   os.exit(1)
end

if listing then listing:close() end
