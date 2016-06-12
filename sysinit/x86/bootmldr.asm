bits 16
org 0x8000

Entry:
	mov word[ReadFileAddr], dx

	cld
	cli
	mov sp, 0x8000
	call SetupUnrealMode
	sti

	mov si, MsgLoading
	call PrintString

   .loadConfigFile:
	mov si, ConfigFileName
	mov ebx, ConfigFileAddr
	call word[ReadFileAddr]
	jnc .prepareConfig
		mov si, MsgConfigNotFound
		call PrintString
		jmp Halt

   .prepareConfig:
	mov byte[ConfigFileAddr+di], 0 ;;make it nullterminated 
	mov si, ConfigFileAddr

	pusha
	mov ah, 1 ;;check if 'C' key pressed
	int 0x16
	jz .parseConfig ;;any key pressed?
		xor ah, ah
		int 0x16
		cmp ah, 0x3F ;;'F5'?
		jne .parseConfig
			call EditConfig
	
   .parseConfig:
	popa
	call ParseConfig

   .endLoading:
	call PrepareLinker
	
Halt:
	xor ah, ah
	int 0x16
	jmp 0xFFFF:0

;; edit config
;; si: config file ptr
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

EditConfig:
	push si
	call StringLength
	mov word[.textoff], cx
	sti

   .loop:
	call .printAll
	call .updateCursor

	xor ah, ah
	int 0x16
	cmp ah, 0x01 ;;escape
	je .done
	cmp ah, 0x0E ;;backspace
	je .removecall
	cmp ah, 0x53 ;;delete
	je .erase
	cmp ah, 0x4B ;;left
	je .leftcall
	cmp ah, 0x4D ;;right
	je .rightcall
	cmp al, 0 ;;no ascii code?
	je .loop
	mov dl, al
	jmp .insertcall ;;else insert

   .leftcall:
	call .left
	jmp .loop

   .left:
	push si
	add si, word[.textoff]
	cmp word[.textoff], 0
	je .rdone
	dec word[.textoff]
	cmp byte[si-1], 10 ;;LF?
	jne .ldone
		dec word[.textoff]
   .ldone:
	pop si
	ret

   .rightcall:
	call .right
	jmp .loop

   .right:
	push si
	add si, word[.textoff]
	cmp byte[si], 0
	je .rdone
	inc word[.textoff]
	cmp byte[si+1], 10 ;;LF?
	jne .rdone
		inc word[.textoff]
   .rdone:
	pop si
	ret

   .insertcall:
	call .insert
	call .right
	jmp .loop

   .removecall:
	cmp word[.textoff], 0
	je .loop
	call .left
   .erase:
	call .remove
	jmp .loop

   .insert:
	push si
	add si, word[.textoff]
	cmp byte[si], 0 ;;at end? -> append
	je .insback
	push si
	call StringLength
	add si, cx
	lea di, [si-1]
	xchg si, di ;;di points to nul, si one before
	mov byte[di+1], 0 ;;new nul
	std
	rep movsb
	cld
	pop si
	mov byte[si], dl ;;write character
   .insend:
	pop si
   	cmp dl, 13 ;;CR?
	jne .insdone
		mov dl, 10 ;;add LF
		inc si
		call .insert
		dec si
		jmp .insdone
   .insdone:
	ret
   .insback:
	mov byte[si], dl
	mov byte[si+1], 0
	jmp .insend

   .remove:
	push si
	add si, word[.textoff]
	cmp byte[si], 0 ;;end
	je .remdone
	push si
	call StringLength
	mov di, si
	inc si
	rep movsb
	pop si
	cmp byte[si], 10 ;;LF?
	jne .remdone
		pop si
		call .remove
		push si
   .remdone:
	pop si
	ret

   .updateCursor:
	push si
	xor dx, dx
	mov cx, word[.textoff]
   .ucloop:
		test cx, cx
		jz .ucdone
		cmp byte[si], 13 ;;CR?
		jne .ucnext
			mov dl, -2
			jmp .ucnext2
	   .ucnext:
		cmp byte[si], 10 ;;LF?
		jne .ucnext2
			inc dh
	   .ucnext2:
		inc si
		inc dl
		cmp dl, 80
		jne .ucnox
			xor dl, dl
			inc dh
	   .ucnox:
		dec cx
		jmp .ucloop
   .ucdone:
	mov ah, 0x02 ;;set cursor
	add dh, 2
	xor bx, bx
	int 0x10
	pop si
	ret

   .printAll:
	push si
	call BlankScreen
	mov si, MsgEditConfig ;;print title
	call PrintString
	pop si
	push si
	call PrintString
	pop si
	ret

   .done:
	cli
	call .printAll
	mov si, MsgNewline
	call PrintString
	mov si, MsgNewline
	call PrintString
	pop si
	ret

   .textoff dw 0

;; parse config file
;; si: config file ptr
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

ParseConfig:
	mov di, ConfigSectionName
	call FindString
	jnc .parseConfig
		mov si, MsgConfigSectionNotFound
		call PrintString
		jmp Halt

   .parseConfig:
	call LoadModules
	ret

;; find and load modules
;; si: config file ptr
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

LoadModules:
	mov dword[.currentAddress], 0x100000 ;;first module at 1MiB
	mov dword[.currentSlot], ModuleEntriesAddr+0x04 ;;first list entry
	mov dword[ModuleEntriesAddr+0x00], 0 ;;num modules

   .next:
	call SkipWhitespaces
	cmp byte[si], 0
	jz .done
	call ReadPair
	push si
	jc .parseerr

   .analyze:
		push si

		mov si, MsgLoadModule1 ;;show some information
		call PrintString
		mov si, TmpKeyBuffer
		call PrintString
		mov si, MsgLoadModule2
		call PrintString
		mov edx, dword[.currentAddress]
		call PrintHexNumber

		mov si, TmpValueBuffer
		mov di, DefaultName
		call CompareStrings ;;check if '= default'
		pop si
		jnc .showargs
			mov byte[TmpValueBuffer], 0 ;;make it empty
			jmp .load
			
	   .showargs:
			mov si, MsgLoadModule3
			call PrintString
			mov si, TmpValueBuffer
			call PrintString

	   .load:
		mov si, MsgNewline
		call PrintString

		mov si, TmpKeyBuffer
		mov ebx, dword[.currentAddress]
		call word[ReadFileAddr] ;;read module file
		jnc .noerr
			mov si, MsgLoadError
			call PrintString
			jmp Halt

	   .noerr:
		inc dword[ModuleEntriesAddr+0x00] ;;increment module count
		mov edx, dword[.currentSlot] ;;get current slot
		mov eax, dword[.currentAddress] ;;get current location
		mov dword[edx], eax ;;store module location
		mov dword[edx+0x04], edi ;;store module size
		add dword[.currentSlot], 70 ;;advance to next slot
		add dword[.currentAddress], edi ;;next module location

		pop si
		jmp .next

   .done:
		ret
   .parseerr:
		mov si, MsgConfigParseError
		call PrintString
		jmp Halt

	.currentAddress dd 0
	.currentSlot dd 0
	
;; read pair
;; si: pointer to pair
;; -> TmpKeyBuffer: key
;; -> TmpValueBuffer: value
;; -> si: pointer to next pair
;; -> carry flags: parse error
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

ReadPair:
	xor bx, bx
   .keyloop:
	lodsb
	test al, al
	jz .parseError
	call isWhitespace
	jc .skipToNext
	cmp al, '='
	je .skipToNext
	mov byte[TmpKeyBuffer+bx], al
	inc bx
	jmp .keyloop
	
   .skipToNext:
	mov byte[TmpKeyBuffer+bx], 0
	dec si
	call SkipWhitespaces
	lodsb
	cmp al, '='
	jne .parseError
	call SkipWhitespaces

	xor bx, bx
   .valueloop:
	lodsb
	test al, al
	jz .end
	call isNewline
	jc .end
	mov byte[TmpValueBuffer+bx], al
	inc bx
	jmp .valueloop
	
   .end:
	mov byte[TmpValueBuffer+bx], 0
	call SkipWhitespaces
	clc
	ret
	
   .parseError:
	stc
	ret
	
;; calculate string length
;; si: asciiz string
;; -> cx: length
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

StringLength:
	push si
	xor cx, cx
   .loopi:
	inc cx
	lodsb
	test al, al
	jnz .loopi
	dec cx
	pop si
	ret
	
;; compare strings
;; si: string1
;; di: string2
;; carry flag: strings are equal
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

CompareStrings:
	call StringLength
	inc cx
	repe cmpsb
	je .equal
	clc
	ret
   .equal:
	stc
	ret
	
;; fint string
;; si: asciiz string
;; di: asciiz substring to find
;; -> si: pointer after first occurrence
;; -> carry flag: not found
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

FindString:
	xchg si, di
	call StringLength
	xchg si, di

   .loopi:
	cmp byte[si], 0
	je .notfound
   
	pusha
	repe cmpsb
	popa
	je .found
	
	inc si
	jmp .loopi
	
   .found:
	add si, cx
	clc
	ret
	
   .notfound:
	stc

	ret

;; blank screen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
BlankScreen:
	mov ah, 0x02
	xor bx, bx ;;set cursor to 0
	xor dx, dx
	int 0x10

;; clear screen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ClearScreen:
	push es
	push word 0xB800 ;;clear video memory
	pop es
	xor di, di
	mov ax, 0x0720
	mov cx, 25*80
	rep stosw
	pop es
	ret

	
;; skip line
;; si: pointer to text
;; -> si: pointer after newline (cr, lf)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

SkipLine:
   .loopi:
	lodsb
	test al, al
	jz .done
	cmp al, 0x10
	jne .loopi
   .done:
	ret
	
;; is whitespace
;;  al: char
;; -> carry flag: is whitespace
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

isWhitespace:
	cmp al, 10
	je .yes
	cmp al, 13
	je .yes
	cmp al, ' '
	je .yes
	clc
	ret
   .yes:
	stc
	ret
	
isNewline:
	cmp al, 10
	je .yes
	cmp al, 13
	je .yes
	clc
	ret
   .yes:
	stc
	ret

;; skip whitespaces
;; si: pointer to text
;; -> si: pointer after whitespaces (cr, lf, space,)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

SkipWhitespaces:
   .loopi:
	lodsb
	call isWhitespace
	jc .loopi
	dec si
   .done:
	ret

;; print string
;; si: asciiz string
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PrintString:
	mov ah, 0x0E
	.loop:
		lodsb
		test al, al
		jz .done 
		int 0x10
		jmp .loop
	.done:
		ret

;; print decimal number
;; edx: number
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PrintDecNumber:
	pusha
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
	popa
	ret
		
;; print hexadecimal number
;; edx: number
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PrintHexNumber:
	pusha
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
	popa
	ret

;; setup unreal mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
SetupUnrealMode:
    cli
	call .enableA20
	push ss
	push es
	push ds
    lgdt [.gdtEnd]
    mov eax, cr0
    or eax, 1
    mov cr0, eax
	jmp .pm
   .pm:
	mov ax, 0x08
	mov ds, ax
	mov es, ax
	mov ss, ax
    mov eax, cr0
    and eax, ~1
    mov cr0, eax
	pop ds
	pop es
	pop ss
    ret
	
   .gdtBegin:
		dd 0, 0 ;;null descriptor
		db 0xFF, 0xFF, 0, 0, 0, 10010010b, 11001111b, 0  ;;data descriptor
		
   .gdtEnd:
		dw .gdtEnd - .gdtBegin - 1
		dd .gdtBegin

   .enableA20:
	in al, 0x92
	cmp al, 0xFF
	je .nofastA20
	or al, 2
	and al, ~1
	out 0x92, al
	ret

	.nofastA20:
	call .emptykb
	mov al, 0xD1
	out 0x64, al
	call .emptykb
	mov al, 0xDF
	out 0x60, al
	call .emptykb
	ret

   .emptykb:
	call .wait
	in al, 0x64
	cmp al, 0xFF
	je .A20done
	test al, 1
	jz .nooutput
	call .wait
	in al, 0x60
	jmp .emptykb
   .nooutput:
	test al, 2
	jnz .emptykb
   .A20done:
	ret

   .wait:
	mov bx,0xFFFF
	.loopstart:
	dec bx
	jnz .loopstart
	ret

;; initialize pmode and linker
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PrepareLinker:
	mov si, MsgInitGDT
	call PrintString
	lgdt [GDTReg]

	call .initMMap

	mov eax, 0xE000
	mov ebx, 0xD000
	call getTextmodeCursor

	mov ebx, cr0 ;;switch to pmode
	or ebx, 1
	mov cr0, ebx

	jmp 0x08:.pmode
   .pmode:
	mov ax, 0x10
	mov es, ax
	mov ds, ax
	mov ss, ax
	mov esp, LinkerAddr

   .copylinker:
	mov esi, LinkerBegin
	mov edi, LinkerAddr
	mov cx, LinkerEnd-LinkerBegin
	rep movsb

	cli
	hlt
	jmp LinkerAddr

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
	ret

;; get textmode cursor character offset
;; -> ecx cursor location
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

getTextmodeCursor:
  	xor bx, bx
	mov ah, 3
	int 0x10 ;;get cursor position to dl/dh
	movzx bx, dl
	movzx ax, dh
	mov cx, 80
	xor dx, dx
	mul cx ;;y *= 80 (screen width)
	add ax, bx ;;y*80 + x
	movzx ecx, ax
	ret

;; prepare memory map
;; -> 0xE000 memory map
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PrepareMemoryMap:
	mov dword[MemoryMapAddr], 0
	mov di, MemoryMapAddr+0x04

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
		inc dword[es:MemoryMapAddr]  ;;inc num entries

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

	
;; Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

MsgNewline	db 13,10,0
MsgLoading		db "loading modules",13,10,0
MsgLoadModule1	db "load module ",0
MsgLoadModule2	db " to 0x",0
MsgLoadModule3	db " with ",0
DefaultName		db "default",0
MsgEditConfig	db "editing configuration file - press escape to continue",13,10,13,10,0
MsgLoadError		db "module could not be loaded",0
ConfigFileName		db "os3/x86/bootconf.ini",0
MsgConfigNotFound	db "config file not found",13,10,0
ConfigSectionName	db "[bootmodules]",13,10,0
MsgConfigSectionNotFound	db "section [bootmodules] not found",13,10,0
MsgConfigParseError			db "config file parse error",13,10,0

MsgInitGDT db "initialize global descriptor table",13,10,0
MsgInitMMap db "initialize memory map",13,10,0
MsgInitMMapNum db "number of entries in memory map: ",0
MsgInitMMapStart db "start: 0x",0
MsgInitMMapSize db " size: 0x",0
MsgInitMMapUsable db " usable",0
MsgInitMMapUnusable db " unusable",0
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

GDTReg:
	dw 5*8-1
	dd GDTStart


TmpKeyBuffer	equ 0xCB00
TmpValueBuffer	equ 0xCE00
ReadFileAddr	equ 0xCFF0
ConfigFileAddr	equ 0xD000
ModuleEntriesAddr equ 0xF000
MemoryMapAddr equ 0xE000
LinkerAddr	equ 0x10000
StackSize		equ 0x4000

LinkerBegin:
align 4
;%include "output/linker.bin"
align 4
LinkerEnd: