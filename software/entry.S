entry:

# Get hardware thread id
csrr a0, 0xf14

# Set stack pointer to __stackEnd - 4 - (id * 1024)
sll a0, a0, 10
la sp, __stackEnd
sub sp, sp, a0
addi sp, sp, -4

# Jump to main
jal main

# Loop forever
j .
