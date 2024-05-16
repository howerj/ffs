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

run () {
	make run
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
EOF
