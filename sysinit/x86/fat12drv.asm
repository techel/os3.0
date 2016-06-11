[Bits 16]
org 0x7E00

Boot:
	mov bp, 0x7C00
	xor ebx, ebx
	mov word[bp+ReadSectorsOff], dx
   .LoadIt:

	mov si, ImagePath
	mov ebx, 0x8000
	call ReadFile
	jnc .success
	
	mov si, MsgMissing
	call PrintString
	hlt
.success:
	mov dx, ReadFile
	jmp 0x8000
	
TheEnd:
	hlt
	
;; find file in directory sector
;; TmpFNameBuffer: fat12 file name
;; cx: number of sectors
;; ax: starting sector of directory
;; -> ax: starting cluster
;; -> di: file size
;; -> carry flag: not found
;; -> zero flag: 1 if file, 0 when directory or not found
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

FindFile:
   .secloop:
		pusha
		mov cx, 1
		mov bx, TmpDirBuffer
		pusha
		call [bp+ReadSectorsOff]
		popa

		mov cx, 16 ;;16 entries per sector
	   .findloop:
			pusha
			mov si, TmpFNameBuffer
			mov di, bx
			mov cx, 11 ;;file name length
			repe cmpsb
			popa
			jne .notfound
				test byte[bx+0x0B], (1<<3) ;;only volume label
				jz .found
		   .notfound:
			add bx, 32 ;;next entry
			loop .findloop
		
		popa
		inc ax
		loop .secloop
		
	inc cx ;;set zero flag to 0
	stc
	ret
	
.found:
	add sp, 16
	mov ax, word[bx+0x1A]
	mov edi, dword[bx+0x1C]
	test byte[bx+0x0B], 0x10 ;;subdir flag
	clc
	ret

;; read file
;; si: path
;; ebx: destination
;; -> carry flag: failure
;; -> edi: file size
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

ReadFile:
	mov bp, 0x7C00
	push ebx
	xor ebx, ebx
   .findInRootDir:

	call ExtractFileName
	mov ax, word[bp+RootDirOff]
	mov cx, word[bp+RootDirSize]
	call FindFile

	jc .notfound ;;not found
	jz .readfile ;;its a file

   .findInDir:  ;;ax = start cluster of directory
	call ExtractFileName
	.dirClusterLoop:
		push ax
		call .calcSec
		;movzx cx, byte[bp+SecPerClus]
		
		pop dx

		call FindFile
		jz .readfile ;;it is a file, ax = start sector of file
		jnc .findInDir ;;change to subdirectory

		xchg ax, dx
		call .nextFATEntry
		jb .dirClusterLoop ;;more clusters, ax = next cluster
		
		jmp .notfound
	
   .readfile: ;;ax contains cluster
	pop ebx
	push edi
     .fileClusterLoop:
		push ax
		call .calcSec

		;movzx cx, byte[bp+SecPerClus]
		call [bp+ReadSectorsOff]
		pop ax
		
		call .nextFATEntry
		jb .fileClusterLoop ;;more clusters
	pop edi
	clc
	ret
	
.notfound:
	pop ebx
	stc
	ret

.calcSec: ;;ax contains cluster
	movzx cx, byte[bp+SecPerClus]
	mul cx
	add ax, word[bp+DataAreaOff] ;;ax contains data sector
	ret
	
.nextFATEntry:
	push ebx
	call ReadFATEntry
	pop ebx
	cmp ax, 0xFF7
	ret

;; extract from path
;; si: index
;; -> TmpFNameBuffer
;; -> si: points to char after path separator or to terminating character
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

ExtractFileName:
	push ax
	mov bx, 11
   .fillout: ;;fill buffer with spaces
	dec bx
	mov byte[TmpFNameBuffer+bx], ' '
	jnz .fillout
	
	mov di, TmpFNameBuffer
   .loopi:
	lodsb
	test al, al ;;end of string
	jz .done
	
	cmp al, '/'
	je .done
	
	cmp al, '.'
	je .dot

	cmp al, 79
	jb .noc
	sub al, 32 ;;convert to uppercase
	
   .noc:
	stosb
	jnz .loopi
	
   .dot:
	mov di, TmpFNameBuffer+8
	jmp .loopi

   .done:
	pop ax
	ret

;; get FAT entry
;; ax: index
;; -> ax: value 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

ReadFATEntry:
	xor dx, dx
	mov ebx, 1024 ;;1024 entries per 3 sectors
	div bx
	push dx ;;entry offset
	mov cx, 3
	mul cx ;;ax -> fat sector
	
	;mov cx, 3
	add ax, word[bp+FATAreaOff]
	mov bx, TmpFATBuffer
	call [bp+ReadSectorsOff]

	pop cx
	mov bx, cx
	mov ax, cx
	shr ax, 1
	add bx, ax ;;bx = offset*1.5
	mov ax, word[TmpFATBuffer+bx]

	test cx, 1 ;;completely divisible by 2 -> even entry
	jz .even
	
	shr ax, 4 ;;take 3-15
	
   .even:
	and ax, 0xFFF ;;take bits 0-11
	
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

;; Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

MsgMissing db "missing "
ImagePath db "os3/x86/bootmldr.bin", 0

TmpFATBuffer	equ 0x7200
TmpFNameBuffer	equ 0x78E0
TmpDirBuffer	equ 0x7900

;;the following offsets are relative to 0x7C00 (bp)

FATAreaOff		equ ReservedSec
RootDirOff		equ -0x04
RootDirSize		equ -0x08
DataAreaOff		equ -0x0C
ReadSectorsOff	equ -0x20

BytesPerSec		equ 0x0B
SecPerClus		equ 0x0D
ReservedSec		equ 0x0E
NumFATs			equ	0x10
RootEntries		equ 0x11
FATSize			equ 0x16
HiddenSec		equ 0x1C
SecPerTrack		equ 0x18
NumHeads		equ 0x1A