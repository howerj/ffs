\ # Simple Forth File System
\
\ * Author: Richard James Howe
\ * License: Public Domain / The Unlicense
\ * Repo: <https://github.com/howerj/ffs>
\ * Email: <mailto:howe.r.j.89@gmail.com>
\
\ This project contains a file system written for Forth systems
\ that uses Forth Blocks as the mechanism to store and retrieve
\ data meaning that this file should be fairly portable.
\
\ ## Format
\
\ The disk format consists of a series of 1024 byte, aligned,
\ Forth Blocks. The format is optimized to work with Forth
\ blocks and it is expected that a Forth interpreter is
\ available to execute files if needed.
\
\ A file system consists of a maximum of 512 blocks, which is
\ the maximum number of 16-byte entries that can fit in a
\ single block. This limits the maximum file system size of
\ 512KiB. In the future this restriction may be lifted and
\ multiple blocks used to store the FAT, which would raise
\ the limit to approximated 64MIB.
\
\ The FAT is stored as a binary file. Directories are fixed
\ format text files. Files consist of potentially non
\ contiguous Forth Blocks, and thus files must be multiples
\ of 1024 bytes (a limitation that might be removed in the
\ future).
\
\ The file system is designed to be forwards compatible to
\ a degree with various potential changes, or at least offer
\ a possible avenue for an upgrade.
\
\ The file system and utilities are designed for a single
\ user system. This is due to the fact that global variables
\ are used (this could be alleviated by using USER variables)
\ and there is no locking. There is currently no way to 
\ allocate another set of variables to mount a second file
\ system.
\
\ ### FAT - File Allocation Table
\
\ The FAT, or File Allocation Table, is a data-structure at
\ the heart of this file system. It is contained in a single
\ Forth block in this version of the file system and consists
\ of 512 16-bit entries (which limits this file system 512KiB).
\
\ Each 16-bit entry is either a special value or a node in a
\ linked list of entries terminating in a special value.
\
\ Special values include; This block is free, this block is
\ bad (an error occurred reading or writing that block), this
\ block is unmapped (if fewer than 512 blocks are available),
\ or this block is special for some other reason (for example
\ that block holds the FAT itself, the initial directory or
\ the boot block).
\
\ The block entry stored in the FAT table does not necessarily
\ directly correspond to the actual block number as stored on
\ disk. An offset is applied which is useful when the Forth
\ implementation is mapped on to some of the blocks or certain
\ blocks have special meaning (usually the 0 block) and cannot
\ be used.
\
\ The FAT is simply a linked list, it can be traversed in one
\ direction which can slow down seeking. Apart from copying the
\ FAT into a different data-structure that is faster to use,
\ a XOR linked list (see the following:
\ <https://en.wikipedia.org/wiki/XOR_linked_list>)
\ Could be used in lieu of a doubly linked list.
\
\ ### Text Directory Format
\
\ The directory format is incredibly simple, it consists of
\ fixed width text fields that are human and machine readable.
\
\        \ DIRNAME         BLK
\        F FILE.FTH        BLK
\        F FILE.TXT        BLK
\        D DIRECTORY       BLK
\        S FILE.SPC        BLK
\
\ The fields are:
\
\ * Directory entry type, which is two bytes in size.
\ * A file name which is 16 bytes in size.
\ * A block field which is 5 bytes in size.
\
\ The directories must be stored in a compact fashion with no
\ gaps, this matters when entries are removed from the 
\ directory.
\
\ The first entry in a directory contains a special 64-byte
\ field. It has the directory entry type of "\". It contains
\ a copy of the directories name which is used when printing
\ out the present working directory with `pwd`.
\
\ All other directory entries are 32-bytes in size, 23 bytes
\ are currently used. Non used bytes must be set to the
\ space ASCII character.
\
\ File and directory names are 16-bytes in length always, even 
\ when the directory entry is a file name like "ABC.TXT", when 
\ stored as a directory entry the file is padded with trailing 
\ spaces up to the 16 byte limit.
\
\ The "BLK" fields are formatted as 16-bit unsigned hexadecimal 
\ numbers with a "$" prefix. They are the initial entries in a
\ FAT table, which may point to a FAT linked list, a special
\ block, or a sentinel value in the case that the file is a
\ single block in size.
\
\ There is no concept of empty file, all file entries must have
\ an associated block. This may change in the future.
\
\ Directories consist of a single block, which limits their
\ size. This may change in the future.
\
\ Thirty directory entries can fit in a single directory, which
\ is not shown above as each entry being 32 bytes two entries
\ fill and entire line which would push the line length over
\ 64 in this document.
\
\ ### File Format
\
\ A file consists of a entry in a directory and a linked list
\ of blocks in the FAT. All files in this version of the file
\ system must be multiples of 1024 bytes in size. Files do not
\ not have to consist of a contiguous set of blocks, which 
\ marks the major advantage of this file system over 
\ traditional ways of managing Forth blocks.
\
\ Special files are marked with an "S" instead of an "F", many
\ but not all utilities will work with these files, and editing
\ these special files may cause instability.
\
\ ### Special Blocks
\
\ There are at least three special blocks, a boot block, a
\ FAT table, and the root directory. They are the first three
\ blocks in the file system.
\
\ ### Commands
\
\ The commands are implemented as a series of Forth words that
\ use parsing words to take their arguments from the input
\ stream if they have any, that is they do not expect their
\ arguments to be present on the stack.
\
\ There is a help and list of commands that is compiled into
\ the example image that is created at the end of this file,
\ along with an example executable program.
\
\ Commands take their names from Unix and MS-DOS, for example
\ both "ls" and "dir" are present (although they behave
\ slightly differently).
\
\ ### Block Editor
\
\ A block editor is included that works on files stored in a
\ non-contiguous fashion. It can also grow files. The editor
\ commands consist of single or double letter commands, it
\ is described later on, in a help file that is created on
\ disk.
\
\ ## Future Direction
\
\ As with any project there are many things that could be done,
\ only some of which will, writing down these potentialities
\ can be a sort of catharsis in lieu of doing the actual work
\ to resolve them. As to what can be done we could; implement
\ more commands, have optional case insensitivity, raise the
\ limit on some of the file system limits, refactor many of
\ the words, improve error handling and detection, calculate
\ fragmentation and offer a way to defragment the file system,
\ perform error checking on the file systems data-structures
\ and orphaned nodes, offer a way to securely erase files,
\ make a utility in C for manipulating the file system and
\ importing and exporting files to it, parse full paths in
\ commands (e.g. `a/b/c`), allow zero length files, add more
\ file system meta-data, write unit tests, rewrite `list` so
\ we have more control over how things look like, make a better
\ more DOS like shell, differing blocks sizes, and more. 
\
\ The following provides a way to calculate fragmentation:
\ <https://stackoverflow.com/questions/4586972>, which will be
\ very slow on the SUBLEQ system.
\
\ Some file system features there are is no intention to ever
\ implement (such as hard or symbolic links).
\
\ The following is a list of features that I do have the
\ intention of implementing:
\
\ * The File Access Methods
\ (see https://forth-standard.org/standard/file), which can
\ be built upon this file system. One minor modification would
\ be required in that the number of bytes in the last allocated
\ block would need to be stored in the file meta-data.
\ * Using multiple FAT blocks, this would raise the amount of
\ data that could be stored in the file system.
\ * Various optimizations and improvements to the SUBLEQ eForth
\ version, including merging this code back into
\ <https://github.com/howerj/subleq>. More blocks could be
\ mapped in the FAT table, and areas that should not be touched
\ marked as being special. This would allow us to make a 
\ special file that contained the SUBLEQ code in it.
\
\ TODO: 16 byte directory entires, for 64 entries per dir
\ (11 byte dir names...)
\ TODO: Multiple FAT blocks
\ TODO: Better Subleq eForth mapping
\ TODO: Rewrite utilities using the new `read-file` and
\ `write-file` utilities.
\ TODO: Better path parsing?
\ TODO: Test out of memory conditions
\ TODO: Primitive journal? Commit changes to temp blocks and
\ swap out?
\ TODO: Glossary for SUBLEQ eForth as a text file, extension
\ programs for SUBLEQ eForth, ...
\ TODO: Linear block allocation where possible
\ TODO: CRC Utility, 16-bit CCITT 
\ TODO: Lock directories by searching all open file handles,
\ do this to prevent deletions, renaming, and moving files
\ in that directory. Also lock files, only one file handle
\ should be open per file.
\ TODO: SUBLEQ eForth needs to reinitialize data structures on
\ startup, and call `dos`.
\

defined (order) 0= [if]
: (order) ( w wid*n n -- wid*n w n )
  dup if
    1- swap >r recurse over r@ xor
    if 1+ r> -rot exit then rdrop
  then ;
: -order get-order (order) nip set-order ; ( wid -- )
: +order dup >r -order get-order r> swap 1+ set-order ;
[then]

defined eforth [if]
system +order
: wordlist here cell allot 0 over ! ; ( -- wid : alloc wid )
: quine source type cr ; ' quine <ok> !
[else]
use ffs.fb
[then]

defined ?depth 0= [if]
: ?depth depth 1- > -4 and throw ;
[then]

defined du. 0= [if] : du. <# #s #> type ; [then]
defined d- 0= [if] : d- dnegate d+ ; [then]
defined d= 0= [if] : d= rot = -rot = and ; [then]
defined 2swap 0= [if] : 2swap >r -rot r> -rot ; [then]
defined d< 0= [if] 
: d< rot 2dup >
  if = nip nip if 0 exit then -1 exit then
  2drop u< ;
[then]
defined d> 0= [if] : d>  2swap d< ; [then]
defined dabs 0= [if] : dabs s>d if dnegate then ; [then]

: dsignum ( d -- n )
  2dup 0 0 d= if 2drop  0 exit then
       0 0 d< if       -1 exit then
                        1 ;

defined spaces 0= [if]
: spaces begin ?dup 0> while bl emit 1- repeat ;
[then]

defined eforth [if] system +order [then]
defined s" 0= [if]
: s" 
  state @ if postpone $" [ ' count ] literal compile, 
  else 
    [char] " parse tuck here dup >r swap cmove r> swap dup 
    allot align
  then ; immediate
[then]

defined /string 0= [if] 
: /string ( b u1 u2 -- b u : advance string u2 )
  over min rot over + -rot - ;
[then]

: ?\ ?exit postpone \ ;
\ : ?( ?exit postpone ( ;

: lower? 97 123 within ; ( ch -- f )
: upper? 65 91 within ; ( ch -- f )
: >lower dup upper? 32 and xor ; ( ch -- ch )
: >upper dup lower? 32 and xor ;

: icompare ( a1 u1 a2 u2 -- n : string comparison )
  rot
  over >lower swap >lower swap - ?dup 
  if >r 2drop r> nip exit then
  >r
  begin
    r@
  while
    2dup c@ >lower swap c@ >lower swap - ?dup if
      rdrop nip nip exit
    then
    1+ swap 1+ swap
    r> 1- >r
  repeat rdrop 2drop 0 ;

: prefix rot min tuck compare ; ( c1 u1 c2 u2 -- f )
: iprefix rot min tuck icompare ; ( c1 u1 c2 u2 -- f )

defined search 0= [if]
: search ( c1 u1 c2 u2 -- c3 u3 f : find c2/u2 in c1/u1 )
  swap >r >r 2dup
  begin
    dup r@ >= over 0> and
  while
    2dup r> r> 2dup >r >r swap prefix
    0= if rot drop rot drop rdrop rdrop -1 exit then
    +string
  repeat
  2drop rdrop rdrop 0 ;
[then]

: isearch ( c1 u1 c2 u2 -- c3 u3 f : find c2/u2 in c1/u1 )
  swap >r >r 2dup
  begin
    dup r@ >= over 0> and
  while
    2dup r> r> 2dup >r >r swap iprefix
    0= if rot drop rot drop rdrop rdrop -1 exit then
    +string
  repeat
  2drop rdrop rdrop 0 ;

: untype ( c-addr u -- remaining ior )
  begin
    dup
  while
    over key swap c!
    +string
  repeat nip 0 ;

defined b/buf 0= [if] 1024 constant b/buf [then]
defined d>s 0= [if] : d>s drop ; [then]

wordlist constant {ffs} 
{ffs} +order definitions
wordlist constant {dos}

128
dup 1+ swap constant EUNKN ( unknown error )
dup 1+ swap constant EIBLK ( bad block )
dup 1+ swap constant EFILE ( file not found )
dup 1+ swap constant EFULL ( disk full )
dup 1+ swap constant EFSCK ( corrupt datastructure / disk )
dup 1+ swap constant EEXIS ( already exists )
dup 1+ swap constant EDDPT ( directory depth exceeded )
dup 1+ swap constant EFLEN ( file length )
dup 1+ swap constant EDFUL ( directory full )
dup 1+ swap constant EDNEM ( directory not empty )
dup 1+ swap constant ENFIL ( not a file )
dup 1+ swap constant ENDIR ( not a directory )
dup 1+ swap constant EARGU ( invalid argument )
dup 1+ swap constant EINTN ( internal error )
dup 1+ swap constant EINAM ( invalid name )
dup 1+ swap constant EPERM ( permission denied )
dup 1+ swap constant ELOCK ( could not obtain lock )
dup 1+ swap constant ERONY ( attempt to modify read-only FS )
dup 1+ swap constant EHAND ( file I/O error )
dup 1+ swap constant ESEEK ( not seekable )
dup 1+ swap constant ELOCK ( could not acquire lock )
drop

: e>s ( code -- )
  dup EUNKN = if drop s" unknown error" exit then
  dup EIBLK = if drop s" bad block" exit then
  dup EFILE = if drop s" file not found" exit then
  dup EFULL = if drop s" disk full" exit then
  dup EFSCK = if drop s" corrupt disk" exit then
  dup EEXIS = if drop s" already exists" exit then
  dup EDDPT = if drop s" directory depth exceeded" exit then
  dup EFLEN = if drop s" file length" exit then
  dup EDFUL = if drop s" directory full" exit then
  dup EDNEM = if drop s" directory not empty" exit then
  dup ENFIL = if drop s" not a file" exit then
  dup ENDIR = if drop s" not a directory" exit then
  dup EARGU = if drop s" invalid argument" exit then
  dup EINTN = if drop s" internal error" exit then
  dup EINAM = if drop s" invalid name" exit then
  dup EPERM = if drop s" permission denied" exit then
  dup ELOCK = if drop s" already locked" exit then
  dup ERONY = if drop s" read only" exit then
  dup EHAND = if drop s" file i/o error" exit then
  dup ESEEK = if drop s" not seekable" exit then
  dup ELOCK = if drop s" locked" exit then
  drop s" unknown" ;

variable error-level 0 error-level !
: elucidate dup error-level ! ?dup if cr e>s type ." ?" then ;
: error swap if dup elucidate throw then drop ; ( f code -- )

$0000 constant blk.end      \ End of FAT chain
$FFFC constant blk.unmapped \ Unmapped / Not memory
$FFFD constant blk.bad-blk  \ Block is bad
$FFFE constant blk.special  \ Special blocks
$FFFF constant blk.free     \ Block is free to use
16 constant maxname         \ Maximum directory entry length
 8 constant maxdir          \ Maximum directory depth
b/buf constant #rem         \ Default Remaining/Used bytes
create dirstk maxdir cells allot dirstk maxdir cells erase

: namebuf: create here maxname dup allot blank does> maxname ;
namebuf: namebuf
namebuf: findbuf
namebuf: compbuf
namebuf: movebuf

32 constant dirsz                \ Length of directory entry
create dirent-store dirsz allot
variable dirp 0 dirp !           \ Directory Pointer
variable read-only 0 read-only ! \ Make file system read only 
$0100 constant version           \ File System version

defined eforth [if]
variable start 0 start !       \ Starting block
variable end   126 end !       \ End block
65 constant init               \ Initial program block
66 constant fat                \ FAT Block
67 constant dirstart           \ Top level directory
$F000 constant copy-store      \ Used to copy blocks
[else]
variable start 1 start !       \ Starting block
variable end   128 end !       \ End block
0 constant init                \ Initial program block
1 constant fat                 \ FAT Block
2 constant dirstart            \ Top level directory
create copy-store 1024 allot   \ Used to copy blocks
[then]
2 constant dsl                 \ Directory Start Line
16 constant l/blk              \ Lines per block
b/buf l/blk / constant c/blk   \ Columns per block
b/buf dirsz / constant d/blk   \ Directories per block
variable loaded 0 loaded !     \ Loaded initial program?
variable eline 0 eline !       \ Empty link in directory
variable exit-shell 0 exit-shell ! \ Used to exit FS shell
variable grepl 0 grepl !       \ used to store grep length
variable insensitive 0 insensitive ! \ Case insensitivity
variable fatal 0 fatal !

8 constant fopen-max
7 cells constant fhandle-size
create fhandles fhandle-size fopen-max * dup cells allot 
       fhandles swap erase
create reqbuf maxname 1+ allot \ File name as a counted string
create newline 2 c, $D c, $A c, align

  1 constant flg.used
  2 constant flg.ren
  4 constant flg.wen
  8 constant flg.stdin
 16 constant flg.stdout
 32 constant flg.error \ TODO: Use this to indicate problems
 64 constant flg.eof
128 constant flg.mem \ Reserved for memory mapped files

\ Offsets into the file handle structure
: f.flags 0 cells + ; ( File Flags and Options )
: f.head 1 cells + ;  ( Head Block of file )
: f.end  2 cells + ;  ( Bytes in last block )
: f.blk  3 cells + ;  ( Current Block Position )
: f.pos  4 cells + ;  ( Position in bytes within block )
: f.dline 5 cells + ; ( Directory Line of File )
: f.dblk 6 cells + ;  ( Directory Block of File )

defined eforth [if] : numberify number? ; [else]
: numberify ( a u -- d -1 | a u 0 : easier than >number )
  -1 dpl !
  base @ >r
  over c@ [char] - = dup >r if +string then
  over c@ [char] $ = if hex +string 
    ( dup 0= if dup rdrop r> base ! exit then ) 
  then
  2>r 0 dup 2r>
  begin
    >number dup
  while over c@ [char] . <>
    if rot drop rot r> 2drop 0 r> base ! exit then
    1- dpl ! 1+ dpl @
  repeat
  2drop r> if dnegate then r> base ! -1 ;
[then]

: fvalid? dup 0 fopen-max 1+ within 0= throw ; 
: findex fvalid? fhandles swap fhandle-size * + ;
: fundex fhandles - fhandle-size / ;
: locked? ( dir -- f )
  >r 0
  begin
    dup fopen-max <
  while
    dup findex f.dblk @ r@ = if rdrop drop -1 exit then
    1+
  repeat
  drop rdrop 0 ;
: locked!? locked? ELOCK error ; ( dir -- )
: equate insensitive @ if icompare exit then compare ;
: examine insensitive @ if isearch exit then search ;
: ro? read-only @ 0<> ERONY error ; ( -- f )
: nul? count nip 0= ; ( a -- f : is counted word empty? )
: token bl word dup nul? EARGU error ; ( -- b )
: grab ( <word> -- a : get word from input stream  )
  begin bl word dup nul? 0= ?exit drop query again ;
: integer grab count numberify nip ; ( <num> -- n f : get int )
: integer? integer 0= dpl @ 0>= or -$18 and throw ;
: modify read-only @ 0= if update then ;
: save read-only @ 0= if update save-buffers then ;
: block? ( blk -- blk )
  start @ + dup start @ end @ 1+ within 0= EIBLK error ;
: addr? block? [ ' block ] literal catch 0<> if
   \ We could mark the block as being bad so long as it is
   \ not a FAT block, otherwise that is a fatal error.
   -1 fatal ! -1 EIBLK error
 then ; ( blk -- addr )
: eline? eline @ ;
: little-endian base c@ 0<> ; 
cell 2 = little-endian and [if]
: 16! ro? ! modify ;
: 16@ @ ;
[else]
: 16! ro? 2dup c! swap 8 rshift swap 1+ c! modify ;
: 16@ dup c@ swap 1+ c@ 8 lshift or ;
[then]
\ : linkable dup 0 end @ 1+ within ; ( blk -- blk f )
: linkable dup dirstart 1+ end @ 1+ within ; ( blk -- blk f )
: link ( blk -- blk : load next block from FAT )
  linkable 0= if drop blk.end exit then
  2* fat addr? + 16@ ; ( blk -- blk )
: previous ( head-blk prior-to-blk -- blk )
  swap
  begin
    2dup link = if
      nip
      exit
    then 
    link
    dup blk.end =
  until drop ;
: reserve 2* fat addr? + 16! modify ; ( blk blk -- )
: setrange ( val blk u )
  rot >r
  begin
    ?dup
  while
    over r@ swap reserve
    +string
  repeat drop rdrop ;
: btotal end @ start @ - ; ( -- n )
: bcheck btotal 4 < -1 and throw ;
: bblk addr? b/buf blank save ; ( blk -- )
: fblk addr? b/buf erase save ; ( blk -- )
: free? ( -- blk f )
  fat addr? init
  begin
    dup end @ <
  while
    2dup 2* + 16@ blk.free = if nip -1 exit then
    1+
  repeat 2drop 0 0 ;
: balloc ( -- blk : allocate single block )
  free? 0= EFULL error dup blk.end swap reserve save ;
: btally ( blk-type -- n )
  0 fat addr? b/buf 2/ 1- for
    dup 16@ 3 pick = if 
     swap 1+ swap
    then
    2 +
  next drop nip ;
: ballocs ( n -- blk : allocate `n` non-contiguous blocks )
  ?dup 0= EINTN error
  dup 1 = if drop balloc exit then
  dup blk.free btally > EFULL error
  blk.end swap
  1- for
    balloc tuck reserve
  next ;
: bvalid? ( blk -- blk : can we free this block? )
  dup dirstart <= EIBLK error
  dup link blk.special = EIBLK error
  dup end @ >= EIBLK error ;
: bfree ( blk -- : free a linked list ) 
  begin
  dup link swap blk.free swap bvalid? reserve
  dup blk.end = until drop save ; 
: bcount ( blk -- n )
  0 swap begin swap 1+ swap link dup blk.end = until drop ;
: btruncate ( n blk -- )
  >r dup 1 r@ bcount within 0= if rdrop drop exit then
  r@ swap r> swap
  1- for nip dup link
  next bfree blk.end swap reserve save ;
: reserve-range ( blk n -- : force allocate contiguous blocks )
  begin
    ?dup
  while
    dup 1 <= 
    if over blk.end swap 
    else over dup 1+ swap then reserve 
    1- swap 1+ swap
  repeat drop save ;
: fmt.init ( -- )
  init addr? b/buf blank
  s" .( HOWERJ SIMPLE FORTH FILE SYSTEM / DOS ) cr 1 loaded !" 
  init addr? swap cmove save ;
: fmt.fat
  fat addr? b/buf erase
  0 b/buf 2/ 1- for blk.unmapped over reserve 1+ next drop
  init if 0 init 1- reserve-range then
\  blk.special 0 init setrange
  blk.special init reserve
  blk.special fat reserve
  blk.special dirstart reserve
  dirstart 1+
  begin
    end @ start @ - over >
  while
    blk.free over reserve 1+
  repeat
  drop
  save ;
: fmt.blks
  dirstart end @ dirstart - start @ - 1- for
    dup fblk 1+
  next drop ;
: xdump ( blk -- )
  base @ >r hex addr? cr
  c/blk 1- for
    l/blk 1- for
      count 0 <# bl hold # # #> type
    next cr
  next r> base ! drop ;
: apply ( file xt -- : apply execution token to file )
  >r begin dup r@ swap >r execute r> link dup blk.end = until 
  rdrop drop ;
: +list block? list ; ( blk -- )
: +load block? load ; ( blk -- )
: bgrep ( N.B - mcopy must hold search term )
  addr?
  l/blk 1- for
    dup c/blk movebuf drop grepl @ examine nip nip if
      dup c/blk type cr
    then
    c/blk +
  next drop ;

cell 2 = [if] \ limit arithmetic to a 16-bit value
: limit immediate ;  [else] : limit $FFFF and ; [then]

\ http://stackoverflow.com/questions/10564491
\ https://www.lammertbies.nl/comm/info/crc-calculation.html
: ccitt ( crc c-addr -- crc : Poly. 0x1021 AKA "x16+x12+x5+1" )
  c@                         ( get char )
  limit over 8 rshift xor    ( crc x )
  dup  4  rshift xor         ( crc x )
  dup  5  lshift limit xor   ( crc x )
  dup  12 lshift limit xor   ( crc x )
  swap 8  lshift limit xor ; ( crc )
: crc ( c-addr u -- ccitt : 16 bit CCITT CRC )
  $FFFF -rot
  begin ?dup while >r tuck ccitt swap r> +string repeat drop ;

: link-load [ ' +load ] literal apply ; ( file -- )
: link-list [ ' +list ] literal apply ; ( file -- )
: link-blank [ ' bblk ] literal apply ; ( file -- )
: link-xdump [ ' xdump ] literal apply ; ( file -- )
: link-u [ ' u. ] literal apply ; ( file -- )
: link-grep [ ' bgrep ] literal apply ; ( file -- )
: more? key [ 32 invert ] literal and [char] Q = ;
: more> cr ." --- (q)uit? --- " ;
: moar +list more> more? ;
: link-more ( file -- ) 
  begin 
    dup moar if cr ." QUIT" drop exit then link dup blk.end = 
  until drop cr ." EOF" ; 
: fat-end ( blk -- blk : last block in FAT chain )
  begin dup link blk.end = if exit then link again ;
: resolve ; \ TODO: Turn name into dir/line
: gc ; \ TODO: Copy FAT nodes
\ N.B. `fat-append` does not set the appended block to
\ `blk.end`, `balloc` does however. This is so another linked
\ list can be appended. It could set it intelligently 
\ however...
: fat-append fat-end reserve save ; ( blk file -- )
: contiguous? ( blk n -- f : is contiguous range free? )
  begin
    ?dup
  while
    over link blk.free <> if 2drop 0 exit then
    1- swap 1+ swap
  repeat drop -1 ;
: contiguous ( n -- blk f : find contiguous slab )
  ?dup 0= if 0 0 exit then
  >r
  0
  begin
    dup end @ <
  while
    dup r@ contiguous? if rdrop -1 exit then
    1+ ( This could be sped up by incrementing past failure )
  repeat rdrop drop 0 0 ;
: largest ( -- n : largest block that we can allocate )
  0 btotal for
    r@ contiguous nip if r@ max then
  next ;
: cballoc ( n -- blk f : allocate contiguous slab )
  dup contiguous if tuck swap reserve-range -1 exit then 0 ;
: dirp? ( -- )
  dirp @ 0 maxdir within 0= if 0 dirp ! 1 EDDPT error then ;
: (dir) dirp? dirstk dirp @ cells + ;
: pushd (dir) ! 1 dirp +! ; ( dir -- )
: popd dirp @ if -1 dirp +! then (dir) @  ; ( -- dir )
: peekd popd dup pushd ; ( -- dir )
: nlen? dup maxname > -1 and throw ; ( n -- n )
: nclear namebuf blank ; ( -- )
: ncopy nclear nlen? namebuf drop swap cmove ; ( c-addr u )
: fclear findbuf blank ; ( -- )
: fcopy fclear nlen? findbuf drop swap cmove ; ( c-addr u )
: cclear compbuf blank ; ( -- )
: ccopy cclear nlen? compbuf drop swap cmove ; ( c-addr u )
: mclear movebuf blank ; ( -- )
: mcopy mclear nlen? movebuf drop swap cmove ; ( c-addr u )
: .hex base @ >r hex 0 <# # # # # #> type r> base ! ;
: hexp base @ >r hex 0 <# # # # # [char] $ hold #> r> base ! ;
: cvalid ( ch -- f : is character valid for a dir name? )
  dup 47 = if drop 0 exit then
  32 127 within ;
: nvalid? ( c-addr u -- f )
  ?dup 0= if drop 0 exit then
\ over c@ 32 <= if 2drop 0 exit then \ Should check all leading
  2dup s" ." ccopy compbuf equate 0= if 2drop 0 exit then
  2dup s" .." ccopy compbuf equate 0= if 2drop 0 exit then
  begin
   ?dup
  while
   over c@ cvalid 0= if 2drop 0 exit then
   swap 1+ swap 1-
  repeat drop -1 ;
: >la dup 0 d/blk 1+ within 0= -1 and throw dirsz * ;
: index >la swap addr? + ;
: dirent-type! index dup >r c! bl r> 1+ c! save ;
: dirent-type@ index c@ ;
: dirent-name! >r >r 2dup nvalid? 0= EINAM error r> r> 
  index 2 + swap cmove save ;
: dirent-name@ index 2 + maxname ; 
: dirent-blk@ ( blk line -- n )
  index maxname + 2 + 5 numberify 0= throw d>s ;
: dirent-blk!  ( n blk line -- )
  index maxname + 2 + >r hexp r> swap cmove save ;
: dirent-rem@ ( blk line -- n )
  index maxname + 7 + 5 numberify 0= throw d>s ;
: dirent-rem! ( n blk line -- )
  index maxname + 2 + 5 + >r hexp r> swap cmove save ;
: dirent-erase ( blk line )
  >la swap addr? + dirsz blank save ; 
: >copy addr? copy-store b/buf cmove ;
: copy> addr? copy-store swap b/buf cmove modify ;
: >dir >la swap addr? + dirent-store dirsz cmove ; 
: dir> >la swap addr? + dirent-store swap dirsz cmove modify ;
: fmtdir ( c-addr u dir -- )
  dup bblk
  >r fcopy
  [char] \ r@ 0 dirent-type!
  findbuf r@ 0 dirent-name!
  #rem r@ 0 dirent-rem!
  r@ r> 0 dirent-blk! 
  save ;
\ TODO: Prevent finding special dirs, empty file of all spaces
: dir-find ( c-addr u blk -- line | -1 )
  >r fcopy
  dsl ( skip first line at zero, this contains directory info )
  begin
    dup d/blk <
  while
    dup r@ swap dirent-name@ findbuf equate 
    0= if rdrop exit then
    1+
  repeat
  rdrop drop -1 ; 
: namelen ( c-addr u -- n : count until space )
  0 >r
  begin
    dup
  while
    over c@ bl <= if 2drop r> exit then
    r> 1+ >r
    1- swap 1+ swap
  repeat 2drop r> ;
: empty? ( blk -- line | -1 : get empty line )
  addr? dirsz dsl * + dsl ( skip first line )
  begin
   dup d/blk <
  while
   over c@ bl <= if nip exit then
   swap dirsz + swap 1+
  repeat
  2drop -1 ;
: is-unempty? empty? dsl <> ; ( blk -- f )
: fmt.root ( -- : format root directory )
  nclear namebuf dirstart
  fmtdir blk.end dirstart reserve save ;
: /root dirp @ for popd drop next ;
: dir? dirent-type@ [char] D = ; ( dir line -- f )
: special? dirent-type@ [char] S = ; ( dir line -- f)
: file? dirent-type@ [char] F = ; ( dir line -- f)
: (remove) ( dir line f -- )
  2 pick locked!?
  >r 2dup dir? if
    r@ 0= ENFIL error
    2dup dirent-blk@ is-unempty? EDNEM error
  then
  rdrop
  2dup special? 0= if 2dup dirent-blk@ bfree then
  2dup dirent-erase
  >r addr? r@
  dirsz * + dup dirsz + swap b/buf r@ 1+ dirsz * 
  - cmove
  rdrop save ;
 : (rm) ( f --, call narg before )
  >r namebuf peekd dir-find dup 0< EFILE error
  peekd swap r> (remove) ;
: (copy) ( src-blks dst-blks )
  swap
  begin
    dup >copy over copy>
    link swap link swap
    dup
    blk.end =
  until <> throw ;
: cmpblk
  2dup ." BLK:" u. u. cr
  addr? swap addr? swap
  l/blk 1- for
    2dup c/blk swap c/blk equate 0<> if
      2dup
      l/blk r@ - 1- u. ." >>> " c/blk type cr
      l/blk r@ - 1- u. ." <<< " c/blk type cr
    then
    c/blk + swap c/blk + swap
  next 2drop ;
: (cmp) ( blks blks -- )
  2dup 2dup bcount swap bcount min
  1- for
    2dup cmpblk
    link swap link   
  next 2drop
  ( N.B. We could do better diff printing here... )
  dup blk.end <> if ." EXTRA 2nd File: " cr dup link-list then
  swap 
  dup blk.end <> if ." EXTRA 1st File: " cr dup link-list then
  2drop ;

\ TODO: Delete block command
wordlist constant {edlin}
{edlin} +order definitions
variable vista 1 vista ! \ Used to be `scr`
variable head 1 head !
variable line 0 line !
: s save ; ( -- : save edited block )
: q s [ {edlin} ] literal -order ( dos ) ; ( -- : quit editor )
: ? head @ . vista @ . line @ . ; ( -- : print blk and line )
: l vista @ block? list ; ( -- : list current block )
: x q head @ link-load
    [ {edlin} ] literal +order ; ( -- : exe file )
: ia 2 ?depth 
  [ $6 ] literal lshift + vista @ addr? + tib
  >in @ + swap source nip >in @ - cmove tib @ >in ! ;
: a 0 swap ia ; : i a ; ( line --, "line" : insert line at )
: w get-order [ {edlin} ] literal 1 ( -- : list cmds )
     set-order words set-order ; 
: n 0 line ! s vista @ link blk.end = if 
    balloc dup bblk head @ fat-append 
  then
  vista @ link vista ! l ;
: p ( -- : prev block )
  0 line ! s head @ vista @ previous vista ! l ; 
: y vista @ >copy ;
: u vista @ copy> save ;
: z vista @ addr? b/buf blank ; ( -- : erase current block )
: d 1 ?depth >r vista @ addr? r> [ $6 ] literal lshift +
   [ $40 ] literal blank ; ( line -- : delete line )
: - line @ -1 line +! line @ 0< if 0 line ! p then ;
: + line @ a 1 line +! line @ l/blk >= if 0 line ! n then ;
{ffs} +order definitions
: edlin ( BLOCK editor )
  vista ! head ! 0 line ! ( only ) [ {edlin} ] literal +order ; 
{edlin} -order

: .type ( blk line -- )
    2dup dir?     if ." DIR   " then
    2dup file?    if ." FILE  " then 
         special? if ." SPEC  " then ;

: .dir ( blk -- )
  cr
  ( ." /" dup 0 dirent-name@ type cr )
  dsl
  begin
    dup d/blk <
  while
    2dup dirent-type@ bl <= if 2drop exit then
    2dup .type
    2dup dirent-name@ type space
    2 spaces
    2dup special? if
      2dup dirent-blk@ ." *" u. 
    else
      ( previously just `2dup dirent-blk@ bcount u.` )
      2dup dirent-blk@ bcount 1- >r
      2dup dirent-rem@ 0 r> b/buf um* d+ du.
    then
    cr
    1+
  repeat 2drop ;
: narg token count ncopy ; ( "token" -- )
: (entry) ( dir, "file" -- blk line )
  >r narg
  namebuf r@ dir-find dup 0< EFILE error r> swap ;
: (file) ( "file" -- blk )
  peekd (entry)
  2dup dirent-type@ [char] D = ENFIL error
  dirent-blk@ ;
: found? peekd eline? ; ( -- cwd line )
: dfull? empty? dup eline ! -1 = EDFUL error ; ( blk -- )
: full? peekd dfull? ; ( -- : is cwd full? )
: (create) ( -- blk: call narg prior, create or open existing )
  namebuf peekd dir-find dup 
  0>= if peekd swap dirent-blk@ exit then
  drop
  full?
  namebuf found? dirent-name!
  #rem found? dirent-rem!
  balloc dup link-blank found? dirent-blk!
  [char] F found? dirent-type!
  found? dirent-blk@ ;
: (mkfile) ( n -- : `narg` should have name in it )
  >r
  namebuf peekd dir-find 0>= EEXIS error
  namebuf found? dirent-name!
  #rem found? dirent-rem!
  r> ballocs dup link-blank found? dirent-blk!
  [char] F found? dirent-type! ;
: (deltree) ( dir -- : recursive delete of directory )
  >r
  begin
    r@ is-unempty? 0= if rdrop exit then
    ." DEL: " r@ dsl dirent-name@ type cr
    r@ dsl dir? if
      r@ dsl dirent-blk@ recurse
      r@ dsl 1 (remove)
    else
      r@ dsl 0 (remove)
    then
  again ;

: yes? if s" yes" exit then s" no" ;

{dos} +order definitions

: df cr
   loaded @ 0= if ." NO DISK" cr exit then
   ." MOUNTED" cr
   ." BLK SZ:      " b/buf u. cr
   ." READ ONLY?   " read-only @ yes? type cr
   ." START BLK:   " start @ u. cr
   ." INSENSITIVE: " insensitive @ yes? type cr
   ." END BLK:     " end @ u. cr
   ." MAX DIRS:    " maxdir u. cr
   ." MAX:         " end @ start @ - dup . ." / " b/buf * u. cr
   ." FREE:        " blk.free btally dup . ." / " b/buf * u. cr 
   ." BAD BLKS:    " blk.bad-blk btally u. cr 
   ." LARGEST CONTIGUOUS BLOCK: " largest u. cr ;

: fsync save-buffers ;
: halt save 1 exit-shell ! only forth ;
: ls peekd .dir ; 
: dir peekd block? list ;
: mount init block? load 0 dirp ! dirstart pushd ;
: freeze 1 read-only ! ;
: melt 0 read-only ! ;
: fdisk melt bcheck fmt.init fmt.fat fmt.blks fmt.root mount ; 
: rename ( "file" "file" -- )
  ( `locked!?` does not need to be called )
  ro?
  narg
  namebuf peekd dir-find dup >r 0< EFILE error
  narg ( dir-find uses `findbuf` )
  namebuf peekd dir-find 0>= EEXIS error
  findbuf peekd r> dirent-name! ;
: move ( "file" "file" -- )
  ro?
  narg namebuf mcopy 
  movebuf peekd dir-find dup >r dup 0<= EFILE error
  peekd swap >dir
  token count 2dup ncopy
  movebuf namebuf equate 0= if rdrop 2drop exit then
  2dup s" ." equate 0= if rdrop 2drop exit then
  s" .." equate 0= if 
    popd peekd dup locked!? >r pushd
  else
    peekd locked!?
    namebuf peekd dir-find dup 0>= if
      dup peekd swap dir? if
        peekd swap dirent-blk@ >r
      else 1 ENDIR error then
    else \ rename
      rdrop
      drop movebuf peekd dir-find 
      >r namebuf peekd r> dirent-name! exit
    then
  then
  r@ dfull?
  movebuf r@ dir-find 0>= EEXIS error
  r> eline? dir>
  peekd r> 2dup dirent-erase 
  ( compact )
  >r addr? r@
  dirsz * + dup dirsz + swap b/buf r@ 1+ dirsz * 
  - cmove rdrop save ;
: mkdir ( "dir" -- )
  ro?
  dirp @ maxdir >= EDDPT error
  full?
  narg
  namebuf peekd dir-find 0>= EEXIS error
  namebuf found? dirent-name!
  #rem found? dirent-rem!
  balloc dup >r found? dirent-blk!
  [char] D found? dirent-type!
  namebuf r> fmtdir ; 
: copy ( "src" "dst" -- )
  ro?
  full?
  narg
  namebuf peekd dir-find dup >r 0< EFILE error
  peekd r@ dirent-type@ [char] F <> ENFIL error
  narg
  namebuf peekd dir-find 0>= EEXIS error
  peekd r> dirent-blk@ dup bcount ballocs dup >r (copy)
  r> found? dirent-blk!
  [char] F found?  dirent-type!
  namebuf found? dirent-name! ;
: cmp 
  narg namebuf peekd dir-find dup >r 0< EFILE error
  narg namebuf peekd dir-find dup >r 0< EFILE error
  r> peekd swap dirent-blk@
  r> peekd swap dirent-blk@ (cmp) ;
: mkfile ro? full? narg 1 (mkfile) ; ( "file" -- )
: fallocate ro? full? narg integer? (mkfile) ; ( "file" u -- )
: fgrow ( "file" count -- )
  ro?
  narg integer? 
  namebuf peekd dir-find dup >r 0< ENFIL error
  ballocs dup link-blank peekd r> dirent-blk@ fat-append save ;
: ftruncate ( "file" count -- )
  peekd locked!?
  ro?
  narg integer?
  namebuf peekd dir-find dup >r 0< ENFIL error
  peekd r> dirent-blk@ btruncate ;
: mknod ( "file" node -- )
  ro?
  full?
  narg
  integer? >r
  namebuf peekd dir-find 0>= EEXIS error
  namebuf found? dirent-name!
  #rem found? dirent-rem!
  r> found? dirent-blk! 
  [char] S found? dirent-type! ;
: grep ( search file -- )
  token count dup grepl ! mcopy
  narg namebuf peekd dir-find dup 0< EFILE error
  peekd swap dirent-blk@ link-grep ; 
: rm ro? narg 0 (rm) ; ( "file" -- )
: rmdir ro? narg 1 (rm) ; ( "dir" -- )
: cd ( "dir" -- )
  token count 2dup s" ." equate 0= if 2drop exit then
  2dup s" .." equate 0= if 2drop popd drop exit then
  2dup s" /" equate 0= if 2drop /root exit then
  peekd dir-find dup >r 0< EFILE error
  peekd r@ dir? 0= ENDIR error
  peekd r> dirent-blk@ pushd ;
: pwd ( -- : printing present working directory )
  0
  begin
    dup dirp @ <
  while
    dup cells dirstk + @ 0 dirent-name@ 
    over >r namelen r> swap type ." /"
    1+
  repeat drop ;
: tree ( -- : tree view of file system, could be improved )
  cr pwd ls
  dsl
  begin
    dup d/blk <
  while
    peekd over dirent-type@ bl <= if drop exit then
    peekd over dirent-type@ [char] D = if
      peekd over dirent-blk@ pushd recurse popd drop
    then
    1+
  repeat drop ; 
: deltree ( "dir" -- )
  ro?
  narg
  namebuf peekd dir-find dup >r 0< EFILE error
  ." DEL: " peekd r@ dirent-name@ type cr
  peekd r@ dir? 0= if rdrop 0 (rm) exit then
  peekd r@ dirent-blk@ (deltree)
  peekd r> 1 (remove) ;
: help ( -- )
  cr
  cr ." Use `more help.txt` in the root directory for help." 
  cr ." Type `cd /` to get to the root directory and `ls`
  cr ." to view files."
  cr cr ." Otherwise visit <https://github.com/howerj/ffs>"
  cr cr ." Command List: " cr words cr cr ;
: cat (file) link-list ; ( "file" -- )
: more (file) link-more ; ( "file" -- )
: exe (file) link-load  ; ( "file" -- )
: hexdump (file) link-xdump ; ( "file" -- )
: stat ( "file" -- )
  peekd (entry) 
  cr 2dup .type 
  cr dirent-blk@ dup bcount ." BCNT: " u. 
  cr ." BLKS: " link-u ;

\ : defrag ; \ compact disk
\ : chkdsk ;
\ : wc ;

{edlin} +order
: edit ro? narg (create) dup edlin ; ( "file" -- )
{edlin} -order

\ Aliases
: bye halt ;
: chdir cd ;
: cls page ;
: cp copy ;
: del rm ;
: ed edit ;
: exit halt ;
: mv move ;
: quit halt ;
: sh exe ;
: touch mkfile ;
: type cat ;
: diff cmp ;

defined eforth [if]
 .( HERE: ) here u. cr 
\ \ This does not work because `(cold)` resets the vocab
\
\ : reboot {ffs} +order mount (cold) ;
\ ' (cold) 2/ <cold> !
[then]

mount loaded @ 0= [if]
cr .( FFS NOT PRESENT, FORMATTING... ) cr
fdisk
\ Nested brackets ifs do not work in SUBLEQ eFORTH...)
defined eforth 0= ?\ mknod [BOOT] 0
defined eforth 0= ?\ mknod [FAT] 1
defined eforth    ?\ mknod [BOOT] 65
defined eforth    ?\ mknod [FAT] 66
\ defined eforth    ?\ mknod [KERNEL] 0
edit help.txt
+ FORTH FILE SYSTEM HELP AND COMMANDS
+
+ Author: Richard James Howe
+ Repo:   https://github.com/howerj/ffs
+ Email:  howe.r.j.89@gmail.com
+
+ This is a simple DOS like file system for FORTH, it makes
+ it easier to edit and manipulate data than using raw blocks.
+
+ For more information see:
+
+ <https://github.com/howerj/ffs>
+ <https://github.com/howerj/subleq>
+
+ To simplify the implementation only the `move` command can
+ move files and directories into other directories, `move`
+ also handles targets "." and "..", like `cd` does.
+
+ To execute a file type `sh <FILE>` or `exe <FILE>`.
+
+ Commands:
+
+ cat / type <FILE>: display a file
+ cd / chdir <DIR>: change working directory to <DIR>
+ cls: clear screen
+ cmp / diff <FILE> <FILE>: compare two files
+ cp / copy <FILE1> <FILE2>: copy <FILE1> to new <FILE2>
+ deltree <FILE/DIR>: delete a file, or directory recurisvely
+ df: display file system information
+ ed / edit <FILE>: edit a <FILE> with the block editor
+ exe / sh <FILE>: execute source <FILE>
+ fallocate <FILE> <NUM>: make <FILE> with <NUM> blocks
+ fdisk: **WARNING** formats disk deleting all data!
+ fgrow <FILE> <NUM>: grow <FILE> by <NUM> blocks
+ freeze: Freeze file system - Turn on read only mode
+ fsync: save any block changes
+ ftruncate <FILE> <NUM>: truncate <FILE> to <NUM> blocks
+ funlock: Display FS lock and Force unlock of file system 
+ grep <STRING> <FILE>: Search for <STRING> in <FILE>
+ halt / quit / bye: safely halt system
+ help: display a short help
+ hexdump <FILE>: hexdump a file
+ ls / dir : list directory
+ melt: Unfreeze file system - Turn off read only mode
+ mkdir <DIR>: make a directory
+ mknod <FILE> <NUM>: make a special <FILE> with <NUM>
+ more <FILE>: display a file, pause for each block
+ mount: attempt file system mounting
+ move <FILE> <FILE/DIR>: move into directory or rename file
+ pwd: print current working directory
+ rename <DIR/FILE1> <DIR/FILE2>: rename a file or directory
+ rm / del <FILE>: remove a <FILE> and not a <DIR>
+ rmdir <DIR>: remove an empty directory
+ sh / shell: invoke file system shell
+ stat <FILE>: display detailed information on <FILE>
+ touch / mkfile <FILE>: make a new file
+ tree: recursively print directories from the current one
+ 
+ Example commands:
+
+ mkdir test
+ cd test
+ edit HELLO.FTH
+ 0 i .( HELLO, WORLD ) cr
+ s q
+ exe HELLO.FTH
+ rm HELLO.FTH
+ ls
+ halt 
+ 
n 
+ EDITOR COMMANDS
+ This block editor is primitive but usable. It operates on
+ 1024 byte Forth blocks, with 16 lines per block. Some 
+ commands accept a line number or position.
+ q : quit editor
+ s : save work
+ n : move to next block in file, allocating one if needed
+ p : move to previous block in file
+ + <LINE>: insert <LINE>, advance line count, 'n' if needed
+ - : decrement line count, call 'p' if needed, no <LINE>
+ x : execute current file from the start
+ #line d : delete #line
+ l : list current block we are editing
+ w : list commands available to the editor
+ #line a / i <LINE>: insert <LINE> onto #line of current block
+ #line #row ia <LINE>: insert <LINE> into #line at #row
+ ? : display current block
+ z : blank current block
+ y : yank block to storage buffer
+ u : replace screen with storage buffer
+
+ Note that it is possible to delete a file whilst editing it,
+ which is not advised but should not break anything.
+
+ When editing a file with `edit` if the file does not exist
+ it is created.
+
+ A typical session might look like:
+
+ edit hello.fth
+ 0 i \ HELLO WORLD PROGRAM, VERSION #666, RJH, 15/04/2024
+ 1 i .( AHOY THERE WORLD, SALUTATIONS AND WARM GREETINGS ) cr
+ s
+ q
+ exe hello.fth
+
+ This is a block editor, and not a freeform text editor, so
+ it does follow a strict format of 16 lines of text per block.
+
+ "p" and "n" can be used to move forward and backward in the
+ file, a new block is assigned if "n" is at the end of the
+ file.
q
edit demo.fth
+ .( HELLO, WORLD ) cr
+ 2 2 + . cr
q
mkdir home
mkdir bin

.( DONE ) cr
[then]


forth-wordlist +order definitions
: dos ( only ) {dos} +order {ffs} +order mount {ffs} -order ;

defined eforth 0= [if]
\ If we are running in eForth we want to add the definitions
\ to the main Forth vocabulary, otherwise we want to add them
\ to the `{ffs}` vocabulary so as not to redefine the gforths
\ File Access Words.
\
\ We also want the `move` from FFS to have higher priority than
\ the standard vocabulary word when we are trying to use the
\ file system, which is achieved if add `{ffs}` with `+order`.
{ffs} +order definitions
[then]

\ File Handle Structure
\
\ FLAGS:    16/cell
\ HEAD-BLK: 16/cell
\ BLK-END:  16/cell
\ BLK:      16/cell
\ BLK-POS:  16/cell
\ DIR-LINE: 16/cell
\ DIR-BLK:  16/cell
\
\ TODO: Optionally allow directories to be opened up
\ TODO: Open memory as a file.
\

: set tuck @ or swap ! ; ( u a -- )
: toggle tuck @ xor swap ! ; ( u a -- )
: clear tuck @ swap invert and swap ! ; ( u a -- )
: ferase findex fhandle-size erase ;
: last? link blk.end = ; ( blk -- f )

: flag swap if emit exit then drop ." -" ; ( flg ch -- )
: .flag
  dup flg.used   and [char] U flag
  dup flg.ren    and [char] R flag
  dup flg.wen    and [char] W flag
  dup flg.stdin  and [char] I flag
  dup flg.stdout and [char] O flag
  dup flg.error  and [char] ! flag
  dup flg.eof    and [char] E flag
  drop ;
: .fhandle ( findex -- )
  cr
  dup f.flags @ ." FLG: " dup u. ." -> " .flag cr
  dup f.head  @ ." HED: " u. cr
  dup f.end   @ ." END: " u. cr
  dup f.blk   @ ." BLK: " u. cr
  dup f.pos   @ ." POS: " u. cr
  dup f.dline @ ." DLN: " u. cr
  dup f.dblk  @ ." DBK: " u. cr
  drop ;

wordlist constant {required}

defined holds 0= [if]
: holds begin dup while 1- 2dup + c@ hold repeat 2drop ;
[then]
: entry ( cs -- f )
  dup >r find nip ?dup if rdrop exit then
  <# r> count holds s" create  " holds 0 0 #> evaluate 0 ;
: .require get-order {required} 1 set-order words set-order ;
: required?
  >r get-order {required} +order definitions r> entry >r
  set-order definitions r> ;

: unused? ( -- ptr f : find a free handle if one exists )
  0
  begin
   dup fopen-max <
  while
   dup findex f.flags @ 0= if -1 exit then
   1+
  repeat
  drop -1 0 ;
: take ( -- ptr f : take a free handle if one exists )
  unused? 0= if 0 exit then
  dup ferase
  dup findex f.flags flg.used swap set -1 ;

: r/o flg.ren ; ( -- fam )
: w/o flg.wen ; ( -- fam )
: r/w r/o w/o or ; ( -- fam )
: (stdio) flg.stdout flg.stdin or ;
: stdio r/w (stdio) or ;
: fam? dup stdio invert and 0<> throw ; ( fam -- fam )
: bin fam? ; ( fam -- fam )
: ferror findex f.flags @ flg.error and 0<> ; ( handle -- f )
: fopened? findex f.flags @ flg.used and 0<> ; ( handle -- f )
: feof? findex f.flags @ flg.eof and 0<> ; ( handle -- f )
: fail findex f.flags flg.error swap set ; ( handle -- )
\ : fassert >r ( fhandle -- )
\  r@ fopened? 0= and throw
\  r@ findex f.pos @ r@ findex f.end @ >= and throw
\  rdrop ;


: pack ( c-addr u )
  reqbuf maxname 1+ blank
  nlen?
  dup reqbuf c!
  reqbuf 1+ swap cmove ;

: open-file ( c-addr u fam -- fileid ior ) 
  fam?
  >r ncopy namebuf s" ." fcopy findbuf equate 0= if 
     take 0= if rdrop -1 EHAND exit then
     dup findex f.flags
     r> flg.stdin or flg.stdout or swap set
     0
     exit
  then
  namebuf peekd dir-find dup 0< if 
    rdrop drop -1 EFILE exit 
  then
  peekd over dir? if rdrop drop 0 ENFIL exit then
  take 0= if rdrop 2drop -1 EHAND exit then
  dup r> swap >r >r
  findex r> over f.flags set
  >r
  dup r@ f.dline !
  peekd r@ f.dblk !
  peekd over dirent-rem@ r@ f.end !
  peekd over dirent-blk@ dup r@ f.blk ! r@ f.head !
  drop rdrop r> 0 ; 
: create-file ( c-addr u fam -- fileid ior )
  fam? >r 2dup ncopy full? 1 [ ' (mkfile) ] literal catch
  ?dup if nip nip nip -1 swap rdrop exit then save
  r> open-file ; 
: flush-file ( fileid -- ior )
  save
  dup ferror if 2drop EHAND exit then
  findex >r
  r@ f.flags @ flg.used and 0= if rdrop EHAND exit then
  r@ f.end @ r@ f.dblk @ r@ f.dline @ dirent-rem!
  rdrop 0 ;
: close-file ( fileid -- ior )
  dup findex f.flags @ flg.used and 0= if drop EHAND exit then
  dup flush-file ?dup if nip exit then ferase 0 ; 
: file-size ( fileid -- ud ior ) 
  dup ferror if 2drop 0 0 EHAND exit then
  findex dup f.head @ bcount swap 
  f.end @ >r b/buf um* r> b/buf swap - 0 d- 0 ; 
: refill query -1 ; ( -- flag )
: include-file ( fileid -- )
  dup ferror EHAND error
  findex 
  dup f.flags @ flg.ren and 0= EPERM error
  dup f.flags @ flg.stdin and 0<> EPERM error
  f.head @ link-load ; 
: included ( c-addr u -- )
  ncopy namebuf peekd dir-find dup 0< EFILE error
  peekd swap dirent-blk@ link-load ; 
: include exe ; ( "file" -- )
: required ( c-addr u -- )
  pack reqbuf dup count mcopy required? ?exit reqbuf count
  included ;
: require ( "name" -- )
  token dup count mcopy
  required? ?exit movebuf included ;
: rename-file ( c-addr1 u1 c-addr2 u2 -- ior ) 
  mcopy ncopy
  namebuf peekd dir-find dup >r 0< if rdrop EFILE exit then
  movebuf peekd dir-find 0>= if rdrop EEXIS exit then
  findbuf peekd r> dirent-name! 0 ;
: delete-file ( c-addr u -- ior )
 ncopy 0 [ ' (rm) ] literal catch ?dup if nip then ; 
: file-position ( fileid -- ud ior ) 
  dup ferror if 2drop 0 0 EHAND exit then
  findex >r
  r@ f.flags @ (stdio) and 0<> if rdrop 0 0 ESEEK exit then
  0 r@ f.head @ begin
    dup r@ f.blk @ <>
  while
    link dup blk.end = if 2drop rdrop 0 0 EHAND exit then
    swap 1+ swap
  repeat drop 
  b/buf um*
  r@ f.pos @ 0 d+ rdrop 0 ;
: file-status ( c-addr u -- x ior )
  r/o open-file ?dup if exit then
  close-file 0 swap ;

: nlast? f.blk @ link blk.end = ;
: limit? dup nlast? if f.end @ exit then drop b/buf ;
: remaining dup >r f.pos @ r> limit? swap - ;
: nblock ( u findex -- f )
  >r r@ f.pos +!
  r@ f.pos @ r@ limit? >= if
    r@ nlast? if
      flg.eof r@ f.flags set
      rdrop 0 exit
    then
    r@ limit? r@ f.pos @ swap - r@ f.pos !
    r@ f.blk @ link r@ f.blk !
  then
  rdrop -1 ;
: read-file ( c-addr u fileid -- u ior )
  over >r >r 2dup erase r>
  findex dup f.flags @ flg.ren and
  0= if rdrop 2drop drop 0 EPERM exit then
  dup f.flags @ flg.stdin and if rdrop drop untype exit then
  >r
  begin
    ?dup
  while ( c-addr u )
    r@ remaining over min ( c-addr u min )
    r@ f.blk @ addr? r@ f.pos @ + ( c-addr u min baddr )
    swap ( c-addr u baddr min ) 3 pick swap
    dup >r cmove r>
    dup r@ nblock
    0= if 
      \ TODO: Test this is correct
      rdrop nip r> min 0 exit
    then
    /string
  repeat
  rdrop drop r> 0 ;

: read-line ( c-addr u fileid -- u flag ior ) 
  \ TODO: Block oriented I/O, handle stdin, byte oriented I/O
; 

: write-file ( c-addr u fileid -- ior ) 
  over >r
  findex dup f.flags @ flg.wen and 
  0= if rdrop 2drop drop EPERM exit then
  dup f.flags @ flg.stdout and if rdrop drop type 0 exit then
  >r
  begin
    ?dup
  while ( c-addr u )
     dup r@ f.pos + dup b/buf >= if ( c-addr u nu )
       
       r@ nlast? if
       then
       \ Next or allocate block     
       \ Write...
     else ( c-addr u nu )
       r@ nlast? if dup r@ f.end ! then
       r@ f.blk @ addr?
     then

\    \ TODO: Update `f.end` (max f.pos f.end)
\    r@ nlast? if
\      
\    then
\
\    r@ remaining over min ( c-addr u min )
\    r@ f.blk @ addr? r@ f.pos @ + ( c-addr u min baddr )
\    swap ( c-addr u baddr min ) 3 pick swap
\    >r swap r> dup >r cmove modify r>
\    dup r@ nblock
\    0= if 
\      \ TODO: Allocate block
\      balloc r@ f.blk @ fat-append
\    then
\    r@ flush-file throw
\    /string
  repeat
  rdrop drop r> 0 ;

: write-line ( c-addr u fileid -- ior )
  dup >r write-file ?dup if rdrop exit then
  newline count r> write-file ;  
: reposition-file ( ud fileid -- ior ) 
  findex dup >r f.flags flg.eof swap clear
  r@ f.flags @ (stdio) and 0<> if rdrop 2drop ESEEK exit then
  um/mod

  -1 throw
; 

: resize-file ( ud fileid -- ior )
  \ This needs to allocate, do nothing, or free, depending
  \ on the situation.
  findex >r
  r@ f.flags @ (stdio) and 0<> if 
    rdrop 2drop 0 0 ESEEK exit 
  then
  r@ fundex file-position 
  ?dup if rdrop 2>r >r 2drop r> 2r> exit then
  2>r 2dup 2r> d- dsignum dup 0= if \ Do nothing...
    drop 2drop rdrop 0 exit
  then
  \ TODO: Get this working, it's nonsense
\  -1 throw
  0< if \ Truncate
    b/buf um/mod dup r@ f.head @ btruncate
    r@ f.end !
    \ TODO: Set f.blk/f.end
    rdrop
    0
    exit
  then \ Grow
  b/buf um/mod dup ballocs r@ f.head @ fat-append
  r> f.end ! ;

defined eforth 0= [if]
\ File Words Test
s" ." r/w open-file throw ( open special file '.' )
\ TODO: Read/Write from `xt` and parameter.
\ A way of reading from a random source would be neat, as well
\ as an equivalent of /dev/null. This could be handled more
\ generally by allowing reading and writing from and to a
\ a callback that could provide that functionality instead of
\ special casing things.
dup constant stdin
dup constant stdout
dup constant stderr
drop

\ TODO: Move this test code to `t`

create buf1 b/buf allot buf1 b/buf erase
variable handle 0 handle !
s" help.txt" r/w open-file throw handle !

: ncat
  r/o open-file throw
  >r
  begin
    buf1 c/blk r@ read-file
    ?dup if r> close-file drop throw then
    ?dup
  while
    buf1 swap type cr
  repeat r> close-file throw 2drop ;

: .pos handle @ ." POS: " file-position throw ud. cr ;

s" help.txt" ncat

.pos
buf1 b/buf handle @ read-file throw ." READ: " u. cr .pos
.( === READ IN === ) cr
buf1 b/buf type cr
.( === READ IN === ) cr
buf1 b/buf handle @ read-file throw ." READ: " u. cr .pos
.( === READ IN === ) cr
buf1 b/buf type cr
.( === READ IN === ) cr

handle @ close-file throw
[then]

dos
{ffs} +order

