.PHONY: nothing
nothing:

.PHONY: clean
clean:
	make -C rtl clean
	make -C de5 clean
	make -C software clean
	make -C tests clean
