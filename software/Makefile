# Binary utilities
RV_ARCH    = rv32i
RV_CC      = riscv64-unknown-elf-gcc
RV_LD      = riscv64-unknown-elf-ld
RV_OBJCOPY = riscv64-unknown-elf-objcopy

# Compiler and linker flags
CFLAGS  = -mabi=ilp32 -march=$(RV_ARCH) -O2 -I.
LDFLAGS = -melf32lriscv -G 0 

# Directories containing RTL, Quartus project, and scripts
RTL = ../rtl
QP  = ../de5
BIN = ../bin

# C file to compile
HFILES = twine.h
CFILES = main.c
OFILES = $(patsubst %.c,%.o,$(CFILES))

# Sizes of instruction and data memories
INSTR_BYTES = 16384
DATA_BYTES  = 32768

.PHONY: all
all: $(RTL)/InstrMem.hex $(QP)/InstrMem.mif \
     $(RTL)/DataMem.hex $(QP)/DataMem.mif

$(RTL)/InstrMem.hex: InstrMem.ihex
	$(BIN)/ihex-to-img.py InstrMem.ihex hex 0 4 $(INSTR_BYTES) \
    > $(RTL)/InstrMem.hex

$(QP)/InstrMem.mif: InstrMem.ihex
	$(BIN)/ihex-to-img.py InstrMem.ihex mif 0 4 $(INSTR_BYTES) \
    > $(QP)/InstrMem.mif

$(RTL)/DataMem.hex: DataMem.ihex
	$(BIN)/ihex-to-img.py DataMem.ihex hex 0 4 $(DATA_BYTES) \
    > $(RTL)/DataMem.hex

$(QP)/DataMem.mif: DataMem.ihex
	$(BIN)/ihex-to-img.py DataMem.ihex mif 32768 4 $(DATA_BYTES) \
    > $(QP)/DataMem.mif

InstrMem.ihex: out.elf
	$(RV_OBJCOPY) --only-section=.text -O ihex out.elf InstrMem.ihex

DataMem.ihex: out.elf
	$(RV_OBJCOPY) -O ihex --remove-section=.text \
    --set-section-flags .bss=alloc,load,contents out.elf DataMem.ihex

out.elf: entry.S $(OFILES) $(HFILES) link.ld
	$(RV_CC) $(CFLAGS) -Wall -c -o entry.o entry.S
	$(RV_LD) $(LDFLAGS) -T link.ld -o out.elf entry.o $(OFILES)

%.o: %.c $(HFILES)
	$(RV_CC) $(CFLAGS) -Wall -c -o $@ $<

.PHONY: clean
clean:
	rm -f *.o *.elf *.ihex
