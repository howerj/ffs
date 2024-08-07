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


