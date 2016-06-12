bits 16
org 0x7E00

xchg bx, bx
jmp labe

times 2000 db 0

labe:
	add ax, 3