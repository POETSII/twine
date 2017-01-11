# BSV compiler flags
BSC = bsc
BSCFLAGS = -wait-for-license -suppress-warnings S0015 \
           -suppress-warnings G0023 \
           -steps-warn-interval 400000 -check-assert \
           +RTS -K32M -RTS

# Parameters
DEFS = -D LogThreadsPerCore=2 \
       -D ThreadsPerCore=4    \
       -D LogInstrsPerCore=12 \
       -D DeviceFamily='"Stratix V"'

# Top level module
TOPFILE = Core.bsv
TOPMOD = mkCore

$(TOPMOD): *.bsv *.c
	make -C ../software
	$(BSC) $(BSCFLAGS) $(DEFS) -D SIMULATE -sim -g $(TOPMOD) -u $(TOPFILE)
	$(BSC) $(BSCFLAGS) -sim -o $(TOPMOD) -e $(TOPMOD) *.c

$(TOPMOD).v: *.bsv
	make -C ../software
	$(BSC) $(BSCFLAGS) -opt-undetermined-vals -unspecified-to X \
         $(DEFS) -u -verilog -g $(TOPMOD) $(TOPFILE)

.PHONY: sim
sim: $(TOPMOD)

.PHONY: verilog
verilog: $(TOPMOD).v

.PHONY: clean
clean:
	rm -f *.cxx *.o *.h *.ba *.bo *.so *.ipinfo
	rm -f InstrMem.hex DataMem.hex
	rm -f mkCore mkCore.v
