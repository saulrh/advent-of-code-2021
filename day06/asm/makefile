DEFAULT: part1

part1: part1.s
	nasm part1.s -f elf64 -g -o part1.o
	gcc -m64 part1.o -o part1

.PHONY: clean
clean:
	rm part1.o
	rm part1
