lli hello_world.ll
nasm -f elf64 -o hello_world.o hello_world.s
ld -o hello_world hello_world.o