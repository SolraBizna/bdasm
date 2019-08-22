local output, e = io.open(output_filename, "wb")
if not output then
   print(e)
   os.exit(1)
end

output:write("v2.0 raw\n\n")

local highest_addr = mem_base + mem_size - 1
while (memory[highest_addr] == 0 or memory[highest_addr] == nil) and highest_addr >= mem_base do
   highest_addr = highest_addr - 1
end

for n=mem_base, highest_addr do
   output:write(("%04X\n"):format(memory[n]))
end
output:close()
