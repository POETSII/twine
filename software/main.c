#include <twine.h>
#include <stdint.h>

volatile uint32_t* jtagUartDataReg = (uint32_t*) 0x10000000;
volatile uint32_t* jtagUartCtrlReg = (uint32_t*) 0x10000004;

void put_char(char c)
{
  // Busy wait until UART has write capacity
  while ((*jtagUartCtrlReg >> 16) == 0);
  // Send char
  *jtagUartDataReg = (uint32_t) c;
}

int main()
{
  int me = myId();

  if (me == 0) {
    const char* str = "hello world\n";
    for (int i = 0; i < 12; i++) put_char(str[i]);
  }

  return 0;
}
