#!/bin/sh
#
# Author: Richard James Howe
# Email: howe.r.j.89@gmail.com
# License: The Unlicense / Public Domain
# Repo: https://github.com/howerj/ffs
#
# Unit tests for a Block Based Forth File System
#

set +eux

reset () {
	make clean
	make ffs.fb
}

# TODO: Select gforth or subleq
run () {
	make run
	#make forth
}

reset

run << EOF
dos
df
ed 1.txt
+ .( TEST PROGRAM ) cr
+ 
s q
rename 1.txt 2.txt
mkdir a
mkdir b
mkdir c
rmdir b
mkdir b
fallocate d 10
mv d b
mv a b
mv c b
tree
deltree b
df
tree

{ffs} +order

create buf1 b/buf 2* allot buf1 b/buf 2* erase

: ncat
  r/o open-file throw
  >r
  begin
    buf1 c/blk r@ read-file
    ?dup if r> close-file drop throw then
    ?dup
  while
    buf1 swap type
  repeat r> close-file throw ;


s" help.txt" ncat

variable handle 0 handle !
s" help.txt" r/w open-file throw handle !

: .pos handle @ ." POS: " file-position throw ud. cr 
\  handle @ findex .fhandle
;

.pos
buf1 b/buf handle @ read-file throw ." READ: " u. cr .pos
.( === READ IN === ) cr
buf1 b/buf type cr
.( === READ IN === ) cr
buf1 b/buf handle @ read-file throw ." READ: " u. cr .pos
.( === READ IN === ) cr
buf1 b/buf type cr
.( === READ IN === ) cr
buf1 b/buf 300 + handle @ read-file throw ." READ: " u. cr .pos
.( === READ IN === ) cr
buf1 b/buf 300 + type cr

handle @ close-file throw

\ TODO: Testing reading 1024 bytes, 1023 bytes, 1024 bytes 
\ after reading X bytes, 0 bytes, 2000 bytes, and writing as
\ well.

s" demo.fth" r/w open-file throw handle !
: yes ( c-addr u file n )
  1- for
\    ." POS: " 0 pick file-position throw ud. cr
    2 pick 2 pick 2 pick write-file throw
  next
  drop 2drop ;
\ s" ABCD" handle @ write-file throw
 s" 1234567890" handle @ 1000 yes
 handle @ close-file throw

ls

\ s" help.txt" r/w open-file throw handle !
\ 100 0 handle @ resize-file throw
\ ls

\ : setf ( du handle -- )
\  dup >r reposition-file throw
\  r> file-position throw ." pos: " ud. cr ;


EOF


