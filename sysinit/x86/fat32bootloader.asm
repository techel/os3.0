bits 16
org 0x7C5A

Boot:
	cld
	xor ax, ax
	mov es, ax
	mov ds, ax
	cli
	mov ss, ax
	mov sp, 0x7000
	sti
	mov bp, 0x7C00
	
	mov byte[bp+DriveNum], dl

    movzx eax, byte[bp+NumFATs]
    mul dword[bp+FATSize]
	movzx ebx, word[bp+ReservedSec]
    add eax, ebx
	movzx ebx, byte[bp+SecPerClus]
	shl ebx, 1
	sub eax, ebx
    mov dword[bp+DataAreaOff], eax

	call ReadFile

	hlt
.success:
	mov dx, ReadSectors
	jmp 0x7E00
	
TheEnd:
	hlt
	
;; find file in directory sectors
;; si: fat12 file name
;; cx: number of sectors
;; eax: starting sector of directory
;; -> eax: starting cluster
;; -> carry flag: not found
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

FindFile:
   .secloop:
		o32 pusha
		mov cx, 1
		mov bx, TmpDirBuffer
		o32 pusha
		call ReadSectors
		o32 popa
		mov cx, 16 ;;16 entries per sector
	   .findloop:
			pusha
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
		
		o32 popa
		inc eax
		loop .secloop
	
	stc
	ret
	
.found:
	add sp, 32
	mov ax, word[bx+0x14] ;;high 16bit
	shl eax, 16
	mov ax, word[bx+0x1A] ;;low 16bit
	test word[bx+0x0B], (1<<4) ;;subdir flag
	clc
	ret

;; search directory
;; si: file name
;; eax first cluster
;; -> eax: first cluster of found file
;; ZF: directory flag
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

SearchDirectory:
   .findfile:
	push eax
	movzx ecx, byte[bp+SecPerClus]
	mul ecx
	add eax, dword[bp+DataAreaOff]
	call FindFile
	jnc .found
		pop eax
		call ReadFATEntry ;;next cluster
		cmp eax, 0xFFFFFF8 ;;last cluster
		jb .findfile
		jmp NotFound

   .found:
	pop edx
	ret

NotFound:
   	mov si, MsgMissing
	call PrintString
	cli
	hlt

;; read the file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

ReadFile:
	mov eax, dword[bp+RootDirStart]
	mov si, ImageDirName
	call SearchDirectory
	jz NotFound ;;it's a file

	mov si, ImageDir2Name
	call SearchDirectory
	jz NotFound ;;it's a file

	mov si, ImageFileName
	call SearchDirectory
	jnz NotFound ;;it's a directory
	
   .readfile: ;;eax contains cluster
		mov bx, 0x7E00
     .fileClusterLoop:
		push eax

		movzx ecx, byte[bp+SecPerClus] ;;calculate sector
		mul ecx
		add eax, dword[bp+DataAreaOff] ;;eax contains data sector
		call ReadSectors

		mov cx, 0x200
		movzx ax, byte[bp+SecPerClus]
		mul cx
		add bx, ax

		pop eax

		push bx
		call ReadFATEntry ;;next fat entrx
		pop bx
		cmp eax, 0xFFFFFF8
		jb .fileClusterLoop ;;more clusters

	ret

;; get FAT entry
;; eax: index
;; -> eax: value 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

ReadFATEntry:
	push eax
	shr eax, 7 ;;div with 128 entries per sector
	movzx edx, word[bp+FATAreaOff]
	add eax, edx 
	mov cx, 1
	mov bx, TmpFATBuffer
	call ReadSectors
	pop eax
	mov eax, dword[ebx+eax*4]
	ret
	
;; read sectors
;; cx: number of sectors
;; eax: start sector
;; -> ds:bx: buffer to read to
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ReadSectors:
	mov bp, 0x7C00
	mov dl, byte[bp+DriveNum]
	mov si, TmpDAP
	mov word[si+0x00], 0x0010 ;;DAP size and null
	mov word[si+0x02], cx ;;num sectors
	mov word[si+0x04], bx ;;destination offset
	mov dword[si+0x08], eax ;;start sector low 32bit
	xor eax, eax
	mov dword[si+0x0C], eax ;;start sector high 32bit (null)
	mov word[si+0x06], ax ;;destination segment (null)
	mov ah, 0x42
	int 0x13
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

MsgMissing db "no fat32drv.bin",0
ImageDirName db "OS3        "
ImageDir2Name db "X86        "
ImageFileName db "FAT32DRVBIN"

TmpSectorBuffer	equ 0x7000
TmpFATBuffer	equ 0x7200
TmpFNameBuffer	equ 0x78E0
TmpDirBuffer	equ 0x7900

;;the following offsets are relative to 0x7C00 (bp)

FATAreaOff	equ ReservedSec
DataAreaOff	equ -0x1C
DriveNum	equ -0x15
TmpDAP		equ -0x40
BytesPerSec		equ 0x0B
SecPerClus		equ 0x0D
ReservedSec		equ 0x0E
NumFATs			equ	0x10
FATSize			equ 0x24
RootDirStart	equ 0x2C
HiddenSec		equ 0x1C

times 420-($-$$) db 0