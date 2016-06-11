section .text
bits 32

;; initialize idt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

global InitializeIDT
InitializeIDT:
cli
hlt
	mov al, 0x11 ;;remap IRQ 0-15 to 32-47
	out 0x20, al
	out 0xA0, al
	mov al, 0x20
	out 0x21, al
	mov al, 0x28
	out 0xA1, al
	mov al, 0x04
	out 0x21, al
	mov al, 0x02
	out 0xA1, al
	mov al, 0x01
	out 0x21, al
	out 0xA1, al
	xor al, al
	out 0x21, al
	out 0xA1, al
	
	;mov edi, ISRAddr
	;mov ebx, IDTAddr
	xor ecx, ecx
   .fillIDT:
		mov eax, edi
		mov word[ebx+0], ax
		mov word[ebx+2], 0x0008
		mov word[ebx+4], 0x8E00
		shr eax, 16
		mov word [ebx+6], ax
		
		call .createISR
		
		add ebx, 8 ;;next idt entry
		inc ecx
		cmp ecx, 256
		jb .fillIDT

    lidt [.idtr]
    ret
	
	;;edi: dest, advanced by isr size
	;;ecx: isr nr,
   .createISR:
		push ecx

		cmp ecx, 8
		je .noDummyErrorCode
		cmp ecx, 10
		je .noDummyErrorCode
		cmp ecx, 11
		je .noDummyErrorCode
		cmp ecx, 12
		je .noDummyErrorCode
		cmp ecx, 13
		je .noDummyErrorCode
		cmp ecx, 14
		je .noDummyErrorCode
		cmp ecx, 17
		je .noDummyErrorCode
		cmp ecx, 30
		je .noDummyErrorCode
		
		mov esi, .dummyISRDummyCodeBegin
		mov dword[esi+6], ecx
		mov ecx, .dummyISRDummyCodeEnd-.dummyISRDummyCodeBegin
		jmp .finallyMakeISR
		
	   .noDummyErrorCode:
		mov esi, .dummyISRBegin
		mov dword[esi+1], ecx
		mov ecx, .dummyISREnd-.dummyISRBegin
	   
	   .finallyMakeISR:
		rep movsb
		
		pop ecx
		ret
		
   .idtr:
	dw 256*8-1
	;dd IDTAddr

   .dummyISRBegin:
		push dword 0xFFFFFFFF ;;replaced later
		jmp 0x08:.commonISR
   .dummyISREnd:

   .dummyISRDummyCodeBegin:
		push dword 0xBAAAAAAD ;;dummy error code
		push dword 0xFFFFFFFF ;;replaced later
		jmp 0x08:.commonISR
   .dummyISRDummyCodeEnd:
	
   .commonISR:
		xchg bx, bx
		pusha
		push ds
		push es ;;stack: es, ds, edi, esi, ebp, esp, ebx, edx, ecx, eax, isr nr, dummy code, eip, cs, eflags
		mov ax, 0x10
		mov es, ax
		mov ds, ax

		mov eax, dword[CommonISR]
		cmp eax, 0
		je .isrend

		push esp
		call eax

	   .isrend:
		sub eax, 32 ;;send ack to pic; hw ints begin at 32
		cmp eax, 16 ;;16 hw ints
		jae .endEOI
		.masterEOI:
			mov al, 0x20
			out 0x20, al ;;eoi to first pic
			cmp al, 8
			jb .endEOI
			mov al, 0x20
			out 0xA0, al ;;eoi to second pic
		.endEOI:

		pop es
		pop ds
		popa
		add esp, 8 ;;pop isr nr and error code
		iret

;; set common isr routine
;; eax: ptr to cdecl-function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

global RegisterCommonISR
RegisterCommonISR:
	mov dword[CommonISR], eax
	ret

;; set common isr routine cdecl interface
;; eax: ptr to cdecl-function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

global RegisterCommonISRCdecl
RegisterCommonISRCdecl:
	mov eax, dword[esp+4]
	mov dword[CommonISR], eax
	ret

;; data section
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
section .data
CommonISR dd 0