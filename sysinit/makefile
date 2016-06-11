ifneq ($(arch), x86)
    $(error architecture not defined)
endif


all:
	make --directory $(arch) all
	
clean:
	make --directory $(arch) clean
	
install:
	make --directory $(arch) install

	