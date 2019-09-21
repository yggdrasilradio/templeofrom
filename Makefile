# Makefile structure notes
#
# Automatic dependency tracking is enabled. If that goes wrong, running
# "make clean" should sort it out but that should only be an issue when
# changing the contents of the Makefile itself.
#
# There is a generic rule at the bottom that builds a .bin file from the
# first listed dependency of a target. So to build "temple.bin" from
# "main.asm", a rule that reads:
#
# temple.bin: main.asm
#
# should be enough to have it built. If there are special options for
# lwasm for a target, use a target specific variable such as:
#
# temple.bin: LWASMOPTS=-DFOO
#
# Dependencies will be stored in .d files which will be removed when
# a "make clean" is performed. 
#
# When adding a new target whose build creates dependency files, make
# sure to add the dependency file to the following variable. You do
# NOT need to modify this for files that are simply included by other
# files. It is only needed for things that build actual objects of
# some kind and which may have dependencies, like .bin files.
DEPFILES = main.d laser.d

# Allow overriding where lwasm is but default to a basic PATH search
LWASM ?= lwasm

all:	main

# The main game version
.PHONY: main
main: temple.bin
ifneq ("$(wildcard /media/share1/COCO/drive0.dsk)","")
	decb copy -r -2 -b temple.bin /media/share1/COCO/drive0.dsk,TEMPLE.BIN
endif

temple.bin: main.asm

# Game with laser variant
.PHONY: laser
laser: laser.bin
ifneq ("$(wildcard /media/share1/COCO/drive0.dsk)","")
	decb copy -r -2 -b temple.bin /media/share1/COCO/drive0.dsk,TEMPLE.BIN
endif

laser.bin: LWASMOPTS=-DMLASER
laser.bin: main.asm

# Utility targets
run:
	mame coco2b -flop1 /media/share1/COCO/drive0.dsk -ramsize 512k -ui_active -skip_gameinfo -autoboot_delay 1 -autoboot_command 'LOADM"TEMPLE.BIN":EXEC\n'

clean:
	rm -f *.bin *.zip *.d

backup:
	tar -cvf backups/`date +%Y-%m-%d_%H-%M-%S`.tar Makefile *.asm

install:
	rcp temple.bin ricka@rickadams.org:/home/ricka/rickadams.org/downloads/TEMPLE.BIN

upload:
	cd ~/projects
	rm -f ~/projects/temple.zip
	zip -r ~/projects/temple.zip ~/projects/temple
	rcp ~/projects/temple.zip ricka@rickadams.org:/home/ricka/rickadams.org/downloads/temple.zip
	rm -f ~/projects/temple.zip

zip:
	rm -f tor.zip
	zip tor.zip *
	rcp tor.zip ricka@rickadams.org:/home/ricka/rickadams.org/downloads/tor.zip

# Generic rules for building things follow.

# This rule exists so that the dependency tracking stuff only needs to be
# in one place.
%.bin:
	for i in `$(LWASM) $(LWASMOPTS) --dependnoerr $<`; do echo "$@ $*.d: $$i"; done >> $*.d
	echo "$@ $*.d: $<" >> $*.d
	$(LWASM) $(LWASMOPTS) -o $@ $<

# Fetch in the dependency files. "-include" prevents make from
# whining about missing files.
-include $(DEPFILES)
