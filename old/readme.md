# FORTH FILE SYSTEM

* Author: Richard James Howe
* Email: howe.r.j.89@gmail.com
* License: The Unlicense / Public Domain
* Repo: https://github.com/howerj/ffs

A super simple file system based off of FAT, designed around the BLOCK 
word set available for the programming language FORTH. A FORTH BLOCK is
a 1024 byte buffer that is transferred to and from mass storage. Originally,
I wanted a text only system that would leverage the power of the FORTH 
command interpreter to navigate directories (that is, all data structures
would be FORTH words), however it is not that practical to do this.

TODO:

* [ ] Specification
* [ ] Prototype in C
* [ ] Prototype in FORTH
* [ ] Unit tests
* [ ] Make an outline of a textual file system

