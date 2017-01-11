ifndef QUARTUS_ROOTDIR
  $(error Please set QUARTUS_ROOTDIR)
endif

.PHONY: all
all:
	make -C ../rtl verilog
	qsys-generate -syn S5_DDR3_QSYS.qsys
	quartus_sh --flow compile Golden_top.qpf

.PHONY: update-mif
update-mif:
	make -C ../software
	quartus_cdb --update_mif Golden_top.qpf
	quartus_asm Golden_top.qpf

.PHONY: download-sof
download-sof:
	quartus_pgm Golden_top.cdf

.PHONY: clean
clean:
	rm -f *.mif *.rpt *.msg *.summary *.sld *.sopcinfo *.jdi
	rm -f *.pin *.done *.qws *.sof *.csv *.qws *.smsg
	rm -f *.log *.bak
	rm -rf db incremental_db S5_DDR3_QSYS .qsys_edit dse
