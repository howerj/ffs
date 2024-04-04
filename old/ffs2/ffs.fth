\ Forth File System
\
\ TODO:
\	- Implement this!
\	- Make commands, a file system wordlist
\
\ FAT Structure:
\ - Linked list, 1 block, up to 512 entries, 
\	
\ DIR Entry:
\ - 64 entries per block
\ - 16 bytes per entry
\ - Length 2 bytes, FAT block 2 bytes, 1 byte = type,
\   11 bytes for file name (counted string).
\ 
\

only forth definitions hex

use ffs.fb

0    constant fs.root
1    constant fs.fat
2    constant fs.start
FFFF constant fat.reserved
0    constant fat.free
200  constant fs.size
400  constant b/buf

0 constant file.unused
1 constant file.dir
2 constant file.file
3 constant file.exe

variable fs.cwd
variable fs.parent

: 16! w! ;
: 16@ uw@ ;

: fat fs.fat block ;

: mkfs 
  fs.size 1- for r@ block b/buf erase update next
  fat.reserved fat     16! update ( reserve for Root )
  fat.reserved fat 2 + 16! update ( reserve for FAT )
  fs.start fs.cwd ! ( set current working directory )
  0 fs.parent ! ; 

: stat ;
: run ;

: ls 
  fs.cwd @ block
  3F for
    r@ 10 * over + c@ if
      
    then
  next drop ;
\ : fs.find ;
: cd ;
: rm ;
: mkdir ;

\ Simulate a DOS shell
: shell ;

mkfs
bye
