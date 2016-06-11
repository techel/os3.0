all:
	
	make --directory sysinit all
#	make --directory kernel all
#	make --directory driver all
#	make --directory services all
	
clean:
	make --directory sysinit clean
#	make --directory kernel clean
#	make --directory driver clean
#	make --directory services clean

install: all
	make --directory sysinit install
#	make --directory kernel install
#	make --directory driver install
#	make --directory services install

install-image: install

	imdisk -a -f "$(osimg)" -m S:
	xcopy /e /s /y $(osdir) S:
	imdisk -D -m S:
	