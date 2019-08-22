globals("source_registers", "dest_registers", "all_registers", "register_names", "conditions", "reverse_condition_map")

local common_registers = {
   A=1,
   B=2,
   PC=3,
   MA=4,
   MD=5,
   S=6,
   X=7
}

source_registers = {
   ADD=0<<3,
   SUB=1<<3,
   LSL=2<<3,
   LSR=3<<3,
   ROL=4<<3,
   ROR=5<<3,
   AB=6<<3,
   AINC=7<<3,
   BINC=8<<3,
   ADEC=9<<3,
   BDEC=10<<3,
   ASEX=11<<3,
   NOT=12<<3,
   AND=13<<3,
   OR=14<<3,
   XOR=15<<3,
}

dest_registers = {
   Q=0,
}

for k,v in pairs(common_registers) do
   source_registers[k] = v
   dest_registers[k] = v
end

all_registers = {}
register_names = {}

for k,v in pairs(source_registers) do
   register_names[v] = k
   all_registers[k] = v
end

for k,v in pairs(dest_registers) do
   register_names[v] = k
   all_registers[k] = v
end

conditions = {
   AL=0,
   NEVER=1,
   NAL=1,
   MI=2,
   NMI=3,
   PL=4,
   NPL=5,
   Z=6,
   NZ=7
}

reverse_condition_map = {
   [0] = "",
   [1] = ".NEVER",
   [2] = ".MI",
   [3] = ".NMI",
   [4] = ".PL",
   [5] = ".NPL",
   [6] = ".Z",
   [7] = ".NZ",
}
