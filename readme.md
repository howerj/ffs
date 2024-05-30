# Simple Forth File System based upon the BLOCK word-set

* Author: Richard James Howe
* Email: <mailto:howe.r.j.89@gmail.com>
* License: The Unlicense / Public Domain
* Repo: <https://github.com/howerj/ffs>

This is a simple Forth block based file system. See the Forth
file [ffs.fth][] for more information including documentation. 

The file system provides a word-set that allows a user to
interact with a disk image like you would with a
[DOS][] based system.

This project works under <https://gforth.org/> and also 
SUBLEQ eForth (see <https://github.com/howerj/subleq>).

Some quick notes and limitations:

* Files consist of *non-contiguous* Forth Blocks.
* Files still consist of Forth Blocks, so are multiples of
1024 bytes in size.
* The maximum file system size is roughly 64MiB.
* The maximum number of files per directory is 30.
* The file system is reliant on the Forth Block word-set and 
should run on even the most spartan Forth system so long as it 
is present.
* File names are limited to 16 bytes in size.
* Some commands include; edit, exe, ls, rm, rmdir, mkdir, cd,
pwd, tree, cp, rename, cat, hexdump, more, df, halt, fdisk,
stat.
* Full file path parsing is not available and most commands
operate only using the current working directory.
* The file system is designed to run as a single user system,
there is no locking and global variables are used.
* Only one file system can be mounted at a time.

## Examples

The following section contains a few examples of how to use
the file system and associated commands. To run the SUBLEQ
eForth examples you will need [make][] and a [C compiler][]
installed and on your [PATH][].

### GForth

An example session:

	mkdir example
	cd example
	pwd
	edit test.fth \ Start editor commands
	+ .( FIRST BLOCK ) cr
	n
	+ .( SECOND BLOCK ) cr
	s
	q     \ Back to "DOS"
	ls
	exe test.fth
	df
	rm test.fth
	ls

### SUBLEQ eForth

To run the SUBLEQ eForth system you will need to type:

	make disk

This will make a bootable disk image that will work with
the SUBLEQ VM. It will take a while to do so.

The Forth vocabularies may not be loaded by default, in
which case the command `dos` will need to be run, once it
has the examples in the `GForth` section should work normally.

Instead of saving to a file called `ffs.fb` the disk image
`disk.dec` will be updated after exiting from the SUBLEQ
VM cleanly.

The SUBLEQ VM File System only has access to 61 blocks,
which may be improved upon later.

## References

* <https://github.com/howerj/subleq>
* <https://en.wikipedia.org/wiki/File_Allocation_Table>

[ffs.fth]: ffs.fth
[DOS]: https://en.wikipedia.org/wiki/DOS
[make]: https://www.gnu.org/software/make/
[C Compiler]: https://gcc.gnu.org/
[PATH]: https://en.wikipedia.org/wiki/PATH_(variable)
