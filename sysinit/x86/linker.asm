
;; 32bit section
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

bits 32
section .text

incbin "output/sysinit16.bin"
Entry:
	mov ax, 0x10 ;;initialize segment registers
	mov ds, ax
	mov es, ax
	mov fs, ax
	mov gs, ax
	mov ss, ax
	mov esp, dword[0xD004]
	add esp, 0x100000+0x4000 ;;16KiB of stack space after modules

   .done:
	mov eax, dword[0xF000]
	mov dword[CurrentLogCursor], eax

	mov esi, MsgPmodeInited
	call LogText

	cli
	extern InitializeSystem
	call InitializeSystem
	cli
	hlt

;; log text
;; esi: string
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

bits 32
global LogText
LogText:
	cli
	pusha
	mov edi, 0xB8000 ;;video memory
	mov eax, dword[CurrentLogCursor]
	shl eax, 1
	add edi, eax ;;2 bytes per character
   .loopi:
	lodsb ;;get character
	cmp al, 0 ;;test if null
	je .done
	stosb ;;write to video memory
	mov al, 0x07 ;;grey on black
	stosb
	jmp .loopi
   .done:
	sub edi, 0xB8000
	shr edi, 1
	add edi, 80-1 ;;go one line below
	mov eax, edi
	xor edx, edx
	mov ebx, 80
	div ebx ;;x-offset = cursor pos%80
	sub edi, edx ;;sub x-offset
	cmp edi, 80*50;;check if out of screen
	jnae .inscreen ;;else move screen up
		push edi
		mov esi, 0xB8000+80*2 ;;begin at second line
		mov edi, 0xB8000 ;;to first line
		mov ecx, 80*49 ;;entire screen minus one line
		rep movsw
		mov edi, 0xB8000+49*80*2 ;;clear last line
		mov cx, 80
		mov ax, 0x0720 ;;character (space) + color
		rep stosw
		pop edi
		sub edi, 80
   .inscreen:
	mov dword[CurrentLogCursor], edi ;;save current cursor position
	mov al, 0x0F ;;update hardware cursor
	mov dx, 0x3D4
	mov al, 0x0F
	out dx, al
	mov eax, edi
	inc dx
	out dx, al
	mov al, 0x0E
	dec dx
	out dx, al
	mov eax, edi
	xchg al, ah
	inc dx
	out dx, al
	popa
	cli
	ret

;; log text cdecl interface
;; parameter 0: string
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

global LogTextCdecl
LogTextCdecl:
	mov esi, dword[esp+4]
	call LogText
	ret

;; data section
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
section .data
CurrentLogCursor dd 0

section .rodata
MsgPmodeInited db "successfully switched to protected mode",0




