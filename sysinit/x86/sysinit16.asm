org 0x10 ;;located at 0xFFFF:0x10
bits 16

;; 16bit section
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Entry:
	cli
	mov ax, cs
	mov ds, ax
	xor ax, ax
	mov es, ax

	mov si, MsgInitGDT
	call PrintString

	lgdt [GDTReg]

   .initMMap:
	mov si, MsgInitMMap
	call PrintString
	call PrepareMemoryMap

   .printMMap:
	mov si, MsgInitMMapNum
	call PrintString
	mov edx, dword[es:0xE000]
	call PrintDecNumber
	mov si, MsgNewline
	call PrintString

	xor ecx, ecx
   .printMMapEntry:
		cmp ecx, dword[es:0xE000]
		je .MMapDone
		push ecx
		imul ecx, 12

		mov si, MsgInitMMapStart
		call PrintString

		mov edx, [es:0xE004+ecx]
		call PrintHexNumber

		mov si, MsgInitMMapSize
		call PrintString

		mov edx, [es:0xE004+ecx+4]
		call PrintHexNumber

		mov al, byte[es:0xE004+ecx+8]
		cmp al, 0
		jne .unusable
			mov si, MsgInitMMapUsable
			call PrintString
			jmp .nextmmentry
	    .unusable:
   			mov si, MsgInitMMapUsable
			call PrintString
	.nextmmentry:
		mov si, MsgNewline
		call PrintString
		pop ecx
		inc ecx
		jmp .printMMapEntry
   .MMapDone:

   .initLogging:
	mov si, MsgInitLogging
	call PrintString
	call PrepareLogging

	mov ebx, cr0 ;;switch to pmode
	or ebx, 1
	mov cr0, ebx

	push word 0x08
  bits 32
	db 0x66
	push Exit+0x100000-0x10
	db 0x66
	retf ;;jump
  bits 16

;; prepare logging
;; -> 0xF000 cursor location
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PrepareLogging:
  	xor bx, bx
	mov ah, 3
	int 0x10 ;;get cursor position to dl/dh
	movzx bx, dl
	movzx ax, dh
	mov cx, 80
	xor dx, dx
	mul cx ;;y *= 80 (screen width)
	add ax, bx ;;y*80 + x
	movzx eax, ax
	mov dword[es:0xF000], eax
	ret

;; prepare memory map
;; -> 0xE000 memory map
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PrepareMemoryMap:
	mov di, 0xE004

	xor ebx, ebx
	mov edx, 0x0534D4150 ;; 'SMAP'
	mov eax, 0xe820
	mov dword[es:di + 20], 0xFFFF ;; force a invalid acpi entry
	mov ecx, 24 ;; ask for 24 bytes
	int 0x15
	jc .failed ;; carry set on first call means unsupported

	mov edx, 0x0534D4150 ;; Some bios trash this register
	cmp eax, edx ;; on success, eax must have been reset to 'SMAP'
	jne .failed

	test ebx, ebx ;; ebx = 0 implies list is only 1 entry long (worthless)
	je .failed
	jmp .jmpin

   .E820lp:
		mov eax, 0xE820;; eax and ecx get trashed on every int 0x15 call
		mov dword[es:di+20], 0xFFFF;; force a invalid acpi entry
		mov ecx, 24 ;; ask for 24 bytes again
		int 0x15
		jc .done ;;carry set means "end of list already reached"
		mov edx, 0x0534D4150 ;; repair potentially trashed register

   .jmpin:
		jcxz .skipent ;;skip any 0 length entries
		cmp cl, 20 ;;got a 24 byte acpi response?
		jbe .notext
		test byte[es:di+20], 1 ;;'ignore this data' bit clear?
		jz .skipent

   .notext:
		cmp dword[es:di+4], 0 ;;high 32bit of base not null -> somewhere beyond 4 GiB
		je .goodent
		cmp dword[es:di+12], 0 ;;same for size
		je .goodent
		jecxz .skipent ;;if length is 0, skip entry 
   .goodent:
		mov ecx, dword[es:di+8] ;;format entry (32bit len, size, type)
		mov dword[es:di+4], ecx
		cmp dword[es:di+16], 1 ;;usable region
		jne .unusable
		mov dword[es:di+8], 0
		jmp .endconvert
   .unusable:
		mov dword[es:di+8], 1
   .endconvert:
		add di, 12 ;; move to next slot
		inc dword[es:0xE000]  ;;inc num entries

   .skipent:
		cmp ebx, 0 ;; if ebx resets to 0, list is complete
		jne .E820lp
   .done:
		ret

   .failed:
		mov si, MsgFailed
		call PrintString
		cli
		hlt

;; print string
;; si: asciiz string
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PrintString:
    mov ah, 0x0E
    .loop:
        lodsb
        cmp al, 0
        je .done
        int 0x10
        jmp .loop
    .done:
        ret

;; print decimal number
;; edx: number
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PrintDecNumber:
	push eax
	push edx
	push word 0xFF
	mov eax, edx
   .convert_loop:
	xor edx, edx 
	mov ebx, 10
	div ebx ;;eax = edx/10; edx = edx%10
	push dx ;;push remainder
	cmp eax, 0
	jne .convert_loop
   .popoff:
	pop ax
	cmp ax, 0xFF
	je .done
	add al, '0'
	mov ah, 0Eh ;print it out
	int 10h
	jmp .popoff
   .done:
	pop edx
	pop eax
	ret

;; print hexadecimal number
;; edx: number
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PrintHexNumber:
	pushad
	push word 0
	mov cx, 8
   .convert_loop:
	mov bx, dx
	shr edx, 4
	and bx, 0xF
	cmp bx, 9
	ja  .gt9
	add bx, '0'
	jmp .converted
   .gt9:
	add bx, 'A'-10
   .converted:
	push bx
	loop .convert_loop
   .popoff:
	pop ax
	test ax, ax
	je .done
	mov ah, 0Eh ;print it out
	int 10h
	jmp .popoff
   .done:
	popad
	ret

;; data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

MsgInitGDT db "initialize global descriptor table",13,10,0
MsgInitMMap db "initialize memory map",13,10,0
MsgInitMMapNum db "number of entries in memory map: ",0
MsgInitMMapStart db "start: 0x",0
MsgInitMMapSize db " size: 0x",0
MsgInitMMapUsable db " usable",0
MsgInitMMapUnusable db " unusable",0
MsgNewline db 13,10,0
MsgFailed db "failed",13,10
MsgInitLogging db "initialize logging",13,10,0

GDTStart:
		;;null
	dd 0, 0
		;;kernel code
	dw 0xFFFF	;;limit byte 1 and 2
	dw 0		;;base byte 1 and 2
	db 0			;;base byte 3
	db 10011010b	;;access rights
	db 11001111b	;;other stuff and limit byte 3
	db 0		;;base byte 4
		;;kernel data
	dw 0xFFFF
	dw 0
	db 0
	db 10010010b ;;like above but with exec bit clear
	db 11001111b
	db 0
		;;user code
	dw 0xFFFF	;;limit byte 1 and 2
	dw 0		;;base byte 1 and 2
	db 0			;;base byte 3
	db 11111010b	;;access rights, ring 3
	db 11001111b	;;other stuff and limit byte 3
	db 0		;;base byte 4
		;;user data
	dw 0xFFFF
	dw 0
	db 0
	db 11110010b ;;like above but with exec bit clear and ring3 accessible
	db 11001111b
	db 0

GDTReg:
	dw 5*8-1
	dd GDTStart+0x100000-0x10

Exit: