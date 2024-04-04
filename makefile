CFLAGS=-std=c99 -Wall -Wextra -pedantic -O2 -fwrapv

.PHONY: run dump forth clean

run:
	gforth ffs.fth

ffs.fb: ffs.fth
	gforth $<

dump: ffs.fb
	hexdump -C $<

subleq: subleq.c

forth: subleq subleq.dec
	./subleq subleq.dec

clean:
	rm -fv ffs.fb subleq *.exe
