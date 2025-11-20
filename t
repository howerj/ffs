#!/bin/sh
#
# Author: Richard James Howe
# Email: howe.r.j.89@gmail.com
# License: The Unlicense / Public Domain
# Repo: https://github.com/howerj/ffs
#
# Unit tests for a Block Based Forth File System. This really could
# use a lot of work to add more tests, the file system has a large
# surface area and only cursory tests are done, mainly be the image
# generation routines present in `ffs.fth`. If an error is found a
# test should be added here to either document it or prevent a
# regression.
#

set -eu

# Forth Unit Test Framework, this will be run under
# the Forth system to test.
#
# BUG: There is another bug related to quine, where the first letter
# of a previously used word is printed out as well.
#
# We should stress test this system, aiming to handle extreme cases.
#
UNIT=$(
cat <<'EOF'
defined quine [if] ' quine <ok> ! ( eForth only ) [then]
defined [UNIT] [if] [UNIT] [then]
marker [UNIT]
+ffs +dos

defined eforth [if] 
  : def? defined ; 
  : requine [ ' quine ] literal <ok> ! ;
[else] 
  : def? token find ; 
  : requine ;
[then]

: .fail cr source type cr ." [FAIL]" cr bye ;
: .pass cr ." [PASS]" cr bye ;
: +> def? 0= if .fail then catch 0= ?exit .fail ;
: -> def? 0= if .fail then catch 0<> ?exit .fail ;
: exists? token count file-exists? 0<> ?exit .fail ;
: unexists? token count file-exists? 0= ?exit .fail ;
.( UNIT TEST FRAMEWORK LOADED ) cr

EOF
)

FORTH="${1:-gforth}";

case "${FORTH}" in
	gforth) echo "GFORTH testing" ;;
	subleq) echo "SUBLEQ eForth testing" ;;
	*) echo "Invalid test option (either 'gforth' or 'subleq' are allowed): ${FORTH}"; 
		exit 1; ;;
esac

disk() { # Make a SUBLEQ eForth disk image
	if [ ! -f disk.dec ]; then make disk; fi;
}

clean () {
	make clean;
}

target () {
	if [ "${FORTH}" = "gforth" ]; then 
		# Do nothing, `ffs.fb` will be made as part of `make run` and
		# is fast to do. Making `disk.dec` is very slow.
		true;
	else 
		disk; 
	fi;
}

reset () {
	clean;
	target;
}

run () {
	target;
	if [ "${FORTH}" = "gforth" ]; then
		rm -f ffs.fb;
		make run; # Default makefile run target is gforth
	else
		./subleq disk.dec t.dec
	fi;
}

TFILE=$(mktemp)
echo "TFILE: ${TFILE}";
trap 'rm -fv -- "${TFILE}"' EXIT
PROG=$(
cat <<'EOF'
\ df
ls

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

requine
ls
file: 1.fth
| .( SCRIPT 1: make 'x' ) cr
| mkdir x
| ls
| .( SCRIPT 1: make 'y' ) cr
| mkdir y
| ls
;file
require 1.fth
rmdir x
rmdir y
rm 1.fth
ls

requine
file: 2.fth
| .( 2: mkdir x ) cr ls
| mkdir x
| .( 2: cd x ) cr ls
| cd x
| .( 2: touch z ) cr ls
| touch z
| .( 2: mkdir y ) cr ls
| mkdir y
| .( 2: touch q ) cr ls
| touch q
| ls
| pwd
;file
requine
ls
require 2.fth
ls
df
bye
exists? q
exists? z
requine
ls
rm z
ls
rm q
ls
cd ..
rm 2.fth

ls

df
.pass
EOF
)

echo "${UNIT}" "${PROG}" | run | tee "${TFILE}"
grep -v '^\[FAIL\]' "${TFILE}" > /dev/null
grep '^\[PASS\]' "${TFILE}" > /dev/null
rm "${TFILE}"
exit

