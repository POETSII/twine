# Twine

Twine is a standalone multi-threaded RV32I core that should be easy to
incorporate into any FPGA project.  It has a simple 4-stage pipeline
with support for 4 threads, and provides a single Avalon master
interface for connecting to peripherals.  On Terasic's
[DE5-NET](http://de5-net.terasic.com), it runs at over 330MHz and
requires 470 ALMs.

## Getting started

Download, build and install the RISC-V compiler tools from:

  [https://github.com/riscv/riscv-tools](https://github.com/riscv/riscv-tools)

Ensure the `RISCV` environment variable is set and `$RISCV/bin` is in
your `PATH`.

To run the RISC-V test suite, starting from the `twine` directory:

```
  make -C rtl
  cd tests
  ./run.sh
```

To build the software that runs on the core (hello world), starting
from the `twine` directory:

```
  make -C software
```

To build an FPGA bit-file:

```
  make -C de5
```

To download the bit-file onto FPGA:

```
  make -C de5 download-sof
```

To view the output of the software running on the FPGA:

```
  nios2-terminal
```

To update the bit-file after changing the software:

```
  make -C de5 update-mif
```
