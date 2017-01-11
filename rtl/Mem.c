// Copyright (c) Matthew Naylor

// This module provides 4GB of RAM to BlueSim for simulating memory.
// It divides the 4GB into 1M x 4KB pages, and allocates pages on
// demand.  The initial contents is all zeroes, except where specified
// by the file "DataMem.hex". It provides functions to read and write
// 32-bit words.

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <assert.h>

// Globals
uint32_t** ram = NULL;

// Functions
void ramInit();
void ramWrite(uint32_t addr, uint32_t data, uint32_t bitEn);
uint32_t ramRead(uint32_t addr);

// Initialise
void ramInit()
{
  int i;
  ram = (uint32_t**) malloc((1<<20) * sizeof(uint32_t*));
  for (i = 0; i < (1<<20); i++) ram[i] = NULL;
  FILE* fp = fopen("DataMem.hex", "rt");
  if (fp != NULL) {
    int addr = 0;
    for (;;) {
      uint32_t word;
      int n = fscanf(fp, "%x", &word);
      if (n <= 0) break;
      ramWrite(addr, word, 0xffffffff);
      addr += 4;
    }
  }
}

// Write
void ramWrite(uint32_t addr, uint32_t data, uint32_t bitEn)
{
  uint32_t page = addr >> 12;
  uint32_t offset = (addr & 0xfff) >> 2;
  int i;
  if (ram == NULL) ramInit();
  if (ram[page] == NULL) {
    ram[page] = (uint32_t*) malloc((1<<10) * sizeof(uint32_t));
    for (i = 0; i < (1<<10); i++) ram[page][i] = 0;
  }
  uint32_t val = ram[page][offset];
  ram[page][offset] = (data & bitEn) | (val & ~bitEn);
}

// Read
uint32_t ramRead(uint32_t addr)
{
  uint32_t page = addr >> 12;
  uint32_t offset = (addr & 0xfff) >> 2;
  if (ram == NULL) ramInit();
  if (ram[page] == NULL) return 0;
  return ram[page][offset];
}
