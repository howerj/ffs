CFLAGS=-std=c99 -Wall -Wextra -pedantic -O2 -fwrapv
SRC=ffs.fth
IMAGE=subleq.dec

.PHONY: all default help run dump clean disk forth 

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
	@echo "* forth : run unaltered SUBLEQ eFORTH using '${IMAGE}'"
	@echo "* disk  : make an image with '${SRC}' and '${IMAGE}' for SUBLEQ eFORTH and run it"
	@echo "* ro    : make an image with '${SRC}' and '${IMAGE}' for SUBLEQ eFORTH and run it in read only mode"
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

forth: subleq ${IMAGE}
	./subleq ${IMAGE}

disk: subleq disk.dec
	./subleq disk.dec _disk.dec
	mv _disk.dec disk.dec

ro r/o: subleq disk.dec
	./subleq disk.dec

disk.dec: subleq ${IMAGE} ${SRC}
	./subleq ${IMAGE} $@ < ${SRC}

clean:
	git clean -dffx
