\ Forth Block Based File System
\
\ This takes its inspiration from the FAT file system (it is not compatible),
\ it uses the optional BLOCK word-set available in Forth to implement a file
\ system.
\
\ TODO:
\ - Implement file system layer
\ - Add a basic text editor
\ - Put resulting word-sets in different vocabularies
\ - Implement file compression
\ - Make special files for manipulating the file system?
\
use ffs.fb

1024 constant b/buf
666  constant version

\ File system Structure:
\ 1st block   - File System information
\ block N-M   - FAT Table
\ block M+1-O - Data (files/directories)

\ FAT structure
\ Special Values: 0 - free, 1-66532 used, 66533 bad block, 66534 reserved, 66535 end

\ Directory Structure:
\ TYPE/META (2bytes), TIME (2bytes), SIZE (2bytes) (of last block), 
\ FAT (2bytes), NAME(48bytes)

: 16@ w@ ;
: 16! w! ;
: mask $FFFF and ;
: clear 1- for dup block b/buf erase update 1+ next flush drop ; ( k count -- )
variable cwd
variable fs-blk
variable fs-cnt

: mbr fs-blk @ ;
: fat fs-blk @ 1+ ;

: fat-index b/buf mod 2/ fat ; \ Get block index
: fat-size ; \ get file size
: fat-free ; \ get free

: format-fat ; \ calculated needed FAT blocks, fill, 
: format-root-dir ;
: format-mbr
  version mbr block 0 + 16!
  fs-blk  @ mbr block 2 + 16!
  fs-cnt  @ mbr block 4 + 16! 
  S" FORTH FILE SYSTEM" mbr block 32 + swap cmove
  parse-word mbr block 64 + swap cmove
  update flush ;

: format ( k count "name" -- )
  fs-cnt ! fs-blk !  mbr fs-cnt @ clear format-mbr format-fat format-root-dir ; 
: (ls) drop ;
: ls ; ( "path" -- )
: cd ; ( "path" -- )
: rm ;
: run ; ( "path" -- )
: open ;
: read ;
: write ;
: close ;
: seek ;
: stat ;
: pwd cwd @ (ls) ;
: usage ; \ disk stats

1 32 format test-sys

cr
.( Done ) cr
bye
