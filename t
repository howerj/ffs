#!/bin/sh
#
# Author: Richard James Howe
# Email: howe.r.j.89@gmail.com
# License: The Unlicense / Public Domain
# Repo: https://github.com/howerj/ffs
#
# Unit tests for a Block Based Forth File System
#

set -eu

# Forth Unit Test Framework, this will be run under
# the Forth system to test.
UNIT=$(
cat <<'EOF'
marker [UNIT]
+ffs +dos
: .fail cr source type cr ." [FAIL]" cr bye ;
: .pass cr ." [PASS]" cr bye ;
: +> token find 0= if .fail then catch 0= ?exit .fail ;
: -> token find 0= if .fail then catch 0<> ?exit .fail ;
: exists? token count file-exists? 0<> ?exit .fail ;
: unexists? token count file-exists? 0= ?exit .fail ;
EOF
)

FORTH="${1:-gforth}";

case "${FORTH}" in
	gforth) echo "GFORTH testing" ;;
	subleq) echo "SUBLEQ eForth testing" ;;
	*) echo "Invalid test option (either 'gforth' or 'subleq' are allowed): ${FORTH}"; 
		exit 1; ;;
esac

# TODO: Make disk only once (e.g. save it from being cleaned)
disk() {
	if [ ! -f disk.dec ]; then make disk; fi;
}

reset () {
	make clean;
	if [ "${FORTH}" = "gforth" ]; then make ffs.fb; else disk; fi;
}

run () {
	reset
	if [ "${FORTH}" = "gforth" ]; then
		make run;
	else
		./subleq disk.dec _.dec
	fi;
}


#TFILE=$(tempfile)
TFILE=$(mktemp)
trap 'rm -fv -- "${TFILE}"' EXIT
PROG=$(
cat <<'EOF'
unexists? 1.txt
fallocate 1.txt 1
exists? 1.txt
-> mkdir
-> rmdir
\ -> rmdir 1.txt ( fails! )
rm 1.txt
unexists? 1.txt
mkdir a
exists? a
mkdir b
-> rm b
exists? b
exists? a
mv a c
unexists? a
exists? c
ls
file: test.fth
| : example ." HELLO" ;
;file
require test.fth
+> example
rm test.fth
unexists? test.fth


.pass
EOF
)

echo "${UNIT}" "${PROG}" | run | tee "${TFILE}"
grep -v '^\[FAIL\]' "${TFILE}" > /dev/null
grep '^\[PASS\]' "${TFILE}" > /dev/null
rm "${TFILE}"
exit

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

