bits 16
org 0x7C3E

Boot:
	cli
	cld
	xor ax, ax
	mov es, ax
	mov ds, ax
	mov ss, ax
	mov sp, 0x7000
	mov bp, 0x7C00
	
	mov byte[bp+DriveNum], dl
	xchg ebx, ebx

   .PrepareRootDirLocation:
	mov ax, 32 ;;32 byte directory entries
	mul word[bp+RootEntries]
	shr ax, 9 ;;ax /= 512
	mov word[bp+RootDirSize], ax
	xchg bx, ax
    movzx ax, byte[bp+NumFATs]
    mul word[bp+FATSize]
    add ax, word[bp+ReservedSec]
    mov word[bp+RootDirOff], ax
	add ax, bx
	dec ax
	dec ax ;;sub ax, 2
    mov word[bp+DataAreaOff], ax

   .PrepareGeometry:
	mov dl, byte[bp+DriveNum]
	mov ah, 8
	push es
	int 13h
	pop es
	inc dh
	mov byte[bp+NumHeads], dh
	and cx, 0x3F
	mov word[bp+SecPerTrack], cx

   .LoadIt:
   
	call ReadFile
	jnc .success
	
	mov si, MsgMissing
	call PrintString
	hlt
.success:
	mov dx, ReadSectors
	jmp 0x7E00
	
TheEnd:
	hlt
	
;; find file in directory sectors
;; si: fat12 file name
;; cx: number of sectors
;; ax: starting sector of directory
;; -> ax: starting cluster
;; -> carry flag: not found
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

FindFile:
   .secloop:
		pusha
		mov cx, 1
		mov bx, TmpDirBuffer
		pusha
		call ReadSectors
		popa
		
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
		
		popa
		inc ax
		loop .secloop
	
	stc
	ret
	
.found:
	add sp, 16
	mov ax, word[bx+0x1A]
	test ax, (1<<4)
	clc
	ret

;; read the file
;; -> carry flag: failure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

ReadFile:
   .findInRootDir:
	mov si, ImageDirName
	mov ax, word[bp+RootDirOff]
	mov cx, word[bp+RootDirSize]
	call FindFile

	jc .notfound ;;not found
	
   .findInDir1:  ;;ax = start cluster of directory
	.dirClusterLoop1:
		push ax
		call .calcSec
		;movzx cx, byte[bp+SecPerClus]
		
		mov si, ImageDir2Name
		pop dx
		call FindFile
		;;jz .readfile ;;it is a file, ax = start sector of file
		jnc .findInDir2 ;;change to subdirectory

		xchg ax, dx
		call .nextFATEntry

		jb .dirClusterLoop1 ;;more clusters, ax = next cluster
		jmp .notfound

   .findInDir2:  ;;ax = start cluster of directory
	.dirClusterLoop2:
		push ax
		call .calcSec
		;movzx cx, byte[bp+SecPerClus]
		
		mov si, ImageFileName
		pop dx
		call FindFile
		jz .readfile ;;it is a file, ax = start sector of file

		xchg ax, dx
		call .nextFATEntry

		jb .dirClusterLoop2 ;;more clusters, ax = next cluster
		jmp .notfound
	
   .readfile: ;;ax contains cluster
		mov ebx, 0x7E00
     .fileClusterLoop:
		push ax
		call .calcSec
		;movzx cx, byte[bp+SecPerClus]
		call ReadSectors
		pop ax
		call .nextFATEntry
		jb .fileClusterLoop ;;more clusters
	clc
	ret
	
.notfound:
	stc
	ret

.calcSec: ;;ax contains cluster
	movzx cx, byte[bp+SecPerClus]
	mul cx
	add ax, word[bp+DataAreaOff] ;;ax contains data sector
	ret
	
.nextFATEntry:
	call ReadFATEntry
	cmp ax, 0xFF7
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
	call ReadSectors

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
	
;; read sectors
;; cx: number of sectors
;; ax: Starting sector
;; -> ebx: Buffer to read to
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ReadSectors:
	mov bp, 0x7C00
   .nextsec:
		push ax
		push cx
		push ebx
		call .LBA2CHS		; convert starting sector from LBA to CHS
		mov bx, TmpSectorBuffer
   .loopi:
		mov ax, 0x0201			; BIOS read sector (CHS): ah = 0x02; read one sector: al = 0x01
		;mov cl, byte[bp+TmpSector]	 ; sector
		;mov ch, byte[bp+TmpCylinder]   ; track/cylinder
		mov cx, word[bp+TmpSector]
		;mov dl, byte[bp+DriveNum]   ; drive
		;mov dh, byte[bp+TmpHead]	   ; head
		mov dx, word[bp+DriveNum]
		int 0x13 ;;read
		jnc .success			  ; check read error
		xor ah, ah				; INT 0x13, AH=0, DL=0 --> reset disk
		xor dl, dl
		int 0x13
		jmp .loopi				 ; read again
   .success:
		mov si, TmpSectorBuffer ;;si = TmpSectorBuffer
		mov cx, 512
		pop ebx
   .copy:
		lodsb ;;al = [si++]
		mov byte[ebx], al
		inc ebx
		loop .copy
   .nexti:
		pop cx
		pop ax
		inc ax						; queue next sector
		loop .nextsec			   ; read next sector
	cli
	ret
	
.LBA2CHS:
	xor dx, dx						 ; prepare dx:ax for operation
	div word[bp+SecPerTrack]			 ; calculate
	inc dl							 ; adjust for sector 0
	mov byte[bp+TmpSector], dl ;;sector
	xor dx, dx						 ; prepare dx:ax for operation
	div word[bp+NumHeads]				; calculate
	mov byte[bp+TmpCylinder], al ;;cylinder
	mov byte[bp+TmpHead], dl ;;head
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

MsgMissing db "fat12drv.bin missing",0
ImageDirName db "OS3        "
ImageDir2Name db "X86        "
ImageFileName db "FAT12DRVBIN"

TmpSectorBuffer	equ 0x7000
TmpFATBuffer	equ 0x7200
TmpFNameBuffer	equ 0x78E0
TmpDirBuffer	equ 0x7900

;;the following offsets are relative to 0x7C00 (bp)

FATAreaOff	equ ReservedSec
RootDirOff	equ -0x04
RootDirSize	equ -0x08
DataAreaOff	equ -0x0C
TmpSector	equ -0x11
TmpCylinder equ -0x10
DriveNum	equ -0x15
TmpHead		equ -0x14

BytesPerSec		equ 0x0B
SecPerClus		equ 0x0D
ReservedSec		equ 0x0E
NumFATs			equ	0x10
RootEntries		equ 0x11
FATSize			equ 0x16
HiddenSec		equ 0x1C
SecPerTrack		equ 0x18
NumHeads		equ 0x1A

times 448-($-$$) db 0