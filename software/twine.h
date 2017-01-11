#ifndef _TINSEL_H_
#define _TINSEL_H_

// Control/status registers
#define CSR_EMIT       "0x800"
#define CSR_HART_ID    "0xf14"

// Get globally unique thread id of caller
inline int myId()
{
  int id;
  asm ("csrr %0, " CSR_HART_ID : "=r"(id));
  return id;
}

// Emit char to console (simulation only)
inline void emit(char c)
{
  asm volatile("csrw " CSR_EMIT ", %0\n" : : "r"(c));
}

#endif
