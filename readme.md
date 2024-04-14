# Simple Forth File System based upon the BLOCK word-set

* Author: Richard James Howe
* Email: <mailto:howe.r.j.89@gmail.com>
* License: The Unlicense / Public Domain
* Repo: <https://github.com/howerj/ffs>

This is a simple Forth block based file system. See the Forth
file [ffs.fth][] for more information including documentation. 

This project works under <https://gforth.org/> and also 
SUBLEQ eForth (see <https://github.com/howerj/subleq>).

Some quick notes and limitations:

* Files consist of *non-contiguous* Forth Blocks.
* The maximum file system size is 512KiB.
* The maximum number of files per directory is 30.
* The file system is reliant on the Forth Block word-set and 
should run on even the most spartan Forth system so long as it 
is present.
* File names are limited to 16 bytes in size.
* Some commands include; edit, exe, ls, rm, rmdir, mkdir, cd,
pwd, tree, cp, rename, cat, hexdump, more, df, halt, fdisk,
stat.

## References

* <https://github.com/howerj/subleq>
* <https://en.wikipedia.org/wiki/File_Allocation_Table>

[ffs.fth]: ffs.fth
