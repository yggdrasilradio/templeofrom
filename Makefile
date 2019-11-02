
all:	main

main: main.asm
	lwasm -l -9 -b -o temple.bin main.asm > temple.lst
ifneq ("$(wildcard /media/share1/COCO/drive0.dsk)","")
	decb copy -r -2 -b temple.bin /media/share1/COCO/drive0.dsk,TEMPLE.BIN
endif

custom:
	(cd map; ./map.py)
	lwasm -9 -b -o -DMCUSTOM temple.bin main.asm
ifneq ("$(wildcard /media/share1/COCO/drive0.dsk)","")
	decb copy -r -2 -b temple.bin /media/share1/COCO/drive0.dsk,TEMPLE.BIN
endif

laser:
	lwasm -9 -b -o -DMLASER temple.bin main.asm
ifneq ("$(wildcard /media/share1/COCO/drive0.dsk)","")
	decb copy -r -2 -b temple.bin /media/share1/COCO/drive0.dsk,TEMPLE.BIN
endif

run:
	mame coco2b -flop1 /media/share1/COCO/drive0.dsk -ramsize 512k -ui_active -skip_gameinfo -autoboot_delay 1 -autoboot_command 'LOADM"TEMPLE.BIN":EXEC\n'

clean:
	rm -f *.bin *.zip *.lst

backup:
	tar -cvf backups/`date +%Y-%m-%d_%H-%M-%S`.tar Makefile *.asm

install:
	rcp temple.bin ricka@rickadams.org:/home/ricka/rickadams.org/downloads/TEMPLE.BIN
