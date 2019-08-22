all: bdasm.lua $(patsubst %.s, %.compiled.txt, $(wildcard examples/*.s))

bdasm.lua: hcc.lua src/top.lua src/grammar.lua src/registers.lua src/instructions.lua src/assemble.lua src/output.lua
	./$^ > $@ || (rm $@; false)
	chmod +x $@

%.compiled.txt: %.s bdasm.lua
	./bdasm.lua $< $@ $*.listing.txt
