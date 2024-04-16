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
mkdir a
mkdir b
mkdir c
rmdir b
mkdir b
fallocate d 10
move d b
move a b
move c b
tree
deltree b
EOF
