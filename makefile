CFLAGS=-std=c99 -Wall -Wextra -pedantic -O2 -fwrapv
SRC=ffs.fth

.PHONY: all default help run dump clean disk subleq-ffs

default all: help

help:
	@echo "FORTH BLOCK BASED FILE SYSTEM"
	@echo
	@echo "* Author:  Richard James Howe"
	@echo "* License: Public Domain / The Unlicense"
	@echo "* Email:   howe.r.j.89@gmail.com"
	@echo "* Repo:    https://github.com/howerj/ffs"
	@echo
	@echo "Makefile commands:"
	@echo
	@echo "* run   : run gforth on '${SRC}'"
	@echo "* dump  : make 'ffs.fb' and hexdump it"
	@echo "* forth : run unaltered SUBLEQ eFORTH"
	@echo "* disk  : make a disk image using '${SRC}' for SUBLEQ eFORTH"
	@echo "* clean : BANG AND THE DIRT IS GONE"
	@echo
	@echo "For documentation please see '${SRC}' or 'readme.md'"
	@echo

run:
	gforth ${SRC}

ffs.fb: ${SRC}
	echo bye | gforth $<

dump: ffs.fb
	hexdump -C $<

subleq: subleq.c

forth: subleq subleq.dec
	./subleq subleq.dec

disk: subleq disk.dec
	./subleq disk.dec _disk.dec
	mv _disk.dec disk.dec

disk.dec: subleq subleq.dec ${SRC}
	./subleq subleq.dec $@ < ${SRC}

mux: muxleq muxleq.dec ffs.fth
	cat ffs.fth - | ./muxleq muxleq.dec

clean:
	git clean -dffx
