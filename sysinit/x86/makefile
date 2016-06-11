ifndef fs
    fs = fat12
	echo no filesystem set, use fat12
endif

vpath %.asmo output
vpath %.co output
vpath %.bin output

.PHONY: all clean install

all: bootconf.ini $(fs)bootloader.bin $(fs)drv.bin bootmldr.bin

# sysinit.bin: sysinit.asmo idt.asmo sysinit.co pmmgr.co common.co
#	i686-elf-g++ -T linkerscript.txt -lgcc -nodefaultlibs -nostdlib
#	mv -f a.out output\sysinit.bin
	
# sysinit.asmo: sysinit16.bin

%.bin: %.asm
	nasm -X vc -f bin -o output\$@ $<
%.asmo: %.asm
	nasm -X vc -f elf -o output\$@ $<
%.co: %.cpp
	i686-elf-gcc -o output\$@ -c $< -ffreestanding -std=c++11 -nostdlib -fno-rtti -fno-exceptions $(cppflags)

clean:
	del /q output

install:
	mkd -p "$(osdir)\os3"
	mkd -p "$(osdir)\os3\x86"
	cp "./output\$(fs)drv.bin" "$(osdir)\os3\x86\$(fs)drv.bin"
	cp "./output\bootmldr.bin" "$(osdir)\os3\x86\bootmldr.bin"
	cp "./bootconf.ini" "$(osdir)\os3\x86\bootconf.ini"
	
    ifeq ($(fs), fat12)
	    dd bs=1 count=448 seek=62 "if=.\output\fat12bootloader.bin" "of=$(osimg)"
    endif