CFLAGS=-std=c99 -Wall -Wextra -pedantic -O2 -fwrapv

.PHONY: run dump clean subleq-ffs

run:
	gforth ffs.fth

ffs.fb: ffs.fth
	gforth $<

dump: ffs.fb
	hexdump -C $<

subleq: subleq.c

forth: subleq subleq.dec
	./subleq subleq.dec

subleq-ffs: subleq disk.dec
	./subleq disk.dec _disk.dec
	mv _disk.dec disk.dec

disk.dec: subleq subleq.dec ffs.fth
	./subleq subleq.dec $@ < ffs.fth

clean:
	git clean -dffx
