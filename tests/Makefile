# Binary utilities
RV_ARCH    = RV32I
RV_CC      = riscv64-unknown-elf-gcc
RV_LD      = riscv64-unknown-elf-ld
RV_OBJCOPY = riscv64-unknown-elf-objcopy

# Compiler and linker flags
CFLAGS  = -march=$(RV_ARCH) -msoft-float -O2 -I.
LDFLAGS = -melf32lriscv -G 0 

# Directories containing RTL, Quartus project, and scripts
RTL = ../rtl
QP  = ../de5
BIN = ../bin

# Sizes of instruction and data memories
INSTR_BYTES = 16384
DATA_BYTES  = 32768

.PHONY: all
all: $(patsubst %.S,%.code.hex,$(wildcard *.S)) \
     $(patsubst %.S,%.data.hex,$(wildcard *.S))

%.code.hex: %.code.ihex
	$(BIN)/ihex-to-img.py $< hex 0 4 $(INSTR_BYTES) > $@

%.data.hex: %.data.ihex
	$(BIN)/ihex-to-img.py $< hex 0 4 $$((2*$(DATA_BYTES))) > $@

%.code.ihex: %.elf
	$(RV_OBJCOPY) -O ihex --only-section=.text $< $@

%.data.ihex: %.elf
	$(RV_OBJCOPY) -O ihex --remove-section=.text \
                --set-section-flags .bss=alloc,load,contents $< $@

%.elf: %.o link.ld
	$(RV_LD) $(LDFLAGS) -T link.ld -o $@ $<

%.o: %.S test_macros.h riscv_test.h encoding.h
	$(RV_CC) $(CFLAGS) -Wall -c -o $@ $<

.PHONY: clean
clean:
	rm -f *.o *.elf *.out *.ihex *.hex
