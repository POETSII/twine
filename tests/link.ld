OUTPUT_ARCH( "riscv" )

MEMORY
{
  InstrMem : ORIGIN = 0, LENGTH = 0x4000
  DataMem  : ORIGIN = 0x8000, LENGTH = 0x8000
}

__stackEnd = 0x10000;

SECTIONS
{
  .text   : { *.o(.text*) }   > InstrMem
  .bss    : { *.o(.bss*) }    > DataMem = 0
  .rodata : { *.o(.rodata*) } > DataMem
  .sdata  : { *.o(.sdata*) }  > DataMem
  .data   : { *.o(.data*) }   > DataMem
  __heapBase = ALIGN(.);
}
