bits 64
default rel

global main: function

extern printf
extern memcpy
extern memset
extern exit
extern fgetc
extern fopen

section .rodata

format1:
	db '%ld: %ld, %ld', 0x0a, 0x0

format2:
	db 0x0a, 0x0

format3:
	db "total fish: %ld", 0x0a, 0x0

format4:
	db "period: fish, newfish", 0x0a, 0x0

filenotfound:
	db "file not found", 0x0a, 0x0

filename:
	db "../input.txt", 0x0

filemode:
	db "rt", 0x0

section .bss

fish:
	resq 10
newfish:
	resq 10

section .text

newline:
	push rbp
	mov rbp, rsp
	lea rdi, [format2]
	mov rax, 0		;no xmm regs
	call printf WRT ..plt	;trailing newline
	pop rbp
	ret

step:
	push rbp
	mov rbp, rsp
	push r12
	push r13

	lea rdi, [newfish]
	mov qword rsi, 0
	mov qword rdx, 9 * 8
	call memset WRT ..plt	;clear our buffer

	mov qword rax, [fish]		;copy 0 into 6 and 8
	mov qword [newfish + 6*8], rax
	mov qword [newfish + 8*8], rax

	mov rax, 8
	lea r8, [fish]		;index we're copying from
	lea r9, [newfish]	;index we're copying to

step1:
	add r8, 8
	mov qword r10, 0
	add qword r10, [r8]
	add qword r10, [r9]
	mov qword [r9], r10	;newfish + fish+1 -> newfish
	add r9, 8

	dec rax
	jnz step1

	lea rdi, [fish]
	lea rsi, [newfish]
	mov rdx, 10 * 8
	call memcpy WRT ..plt	;newfish -> fish

	pop r13
	pop r12
	pop rbp
	ret

print_fish:
	push rbp
	mov rbp, rsp
	push r12
	push r13
	push r14
	push r15

	lea rdi, [format4]
	mov rax, 0
	call printf WRT ..plt

	lea r12, [fish]
	lea r13, [newfish]
	mov r14, 0
print_fish1:
	lea rdi, [format1]
	mov rsi, r14
	mov qword rdx, [r12]
	mov qword rcx, [r13]
	mov rax, 0		;no xmm regs
	call printf WRT ..plt

	add r12, 8
	add r13, 8
	inc r14
	mov r15, r14
	sub r15, 9
	jnz print_fish1

	pop r15
	pop r14
	pop r13
	pop r12
	pop rbp
	ret

total_fish:
	push rbp
	mov rbp, rsp

	mov rcx, 9
	lea rdx, [fish]
	mov rax, 0
total_fish1:
	add qword rax, [rdx]
	add rdx, 8
	dec rcx
	jnz total_fish1

	lea rdi, [format3]
	mov qword rsi, rax
	mov rax, 0
	call printf WRT ..plt

	pop rbp
	ret

read_file:
	push rbp		;make ourselves a stack frame
	mov rbp, rsp
	push r12
	push r13

	lea rdi, [filename]
	lea rsi, [filemode]
	mov rax, 0
	call fopen WRT ..plt

	jz fail

	mov r12, rax
	lea r13, [fish]

read_file1:
	mov rdi, r12
	call fgetc WRT ..plt
	add rax, 0
	jz read_done
	sub rax, 0x30		;ascii numeral to int
	imul qword rax, 8	;multiply by width of an int
	add rax, r13		;index into fish
	mov qword rcx, [rax]
	inc qword rcx
	mov qword [rax], rcx

	mov rdi, r12
	call fgetc WRT ..plt
	add rax, 0
	jz read_done
	sub rax, ','
	jz read_file1

read_done:
	pop r13
	pop r12
	pop rbp			;close out our stack frame
	ret

fail:
	pop r13
	pop r12
	pop rbp			;close out our stack frame

	lea rdi, [filenotfound]
	mov rax, 0
	call printf WRT ..plt

	mov rdi, 1
	call exit WRT ..plt

section .text.startup

main:
	push rbp		;make ourselves a stack frame
	mov rbp, rsp
	push r12
	push r13

	lea rdi, [fish]
	mov rsi, 0
	mov rdx, 9
	call memset WRT ..plt	;clear our buffer

	lea rdi, [newfish]
	mov rsi, 0
	mov rdx, 9
	call memset WRT ..plt	;clear our buffer

	call read_file

	mov r12, 256
main1:
	call step
	dec r12
	jnz main1

	call print_fish
	call total_fish
	call newline

	pop r13
	pop r12
	pop rbp			;close out our stack frame

	mov rdi, 0		;exit 0
	call exit WRT ..plt
