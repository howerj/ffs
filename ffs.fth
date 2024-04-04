\ Simple Forth File System
\
\ This project is a work in progress.
\
\ ## TO DO
\ 
\ * Implement basic functionality, then expand upon it, making
\ a Forth Disk Operating System. 
\ * Rewrite `list` so we have more control over output?
\ * Shell loop that handles custom error
\ * Write variables to first block
\ * Directory wordlist, define "D"/"T" and "F" multiple
\ times in different wordlists
\ * C utility for manipulating disk images / collating
\ files.
\ * Documentation including file system limitations
\ The first block contains executable code and is executed in
\ an attempt to mount the file system.
\ * Use $" instead of s"?
\ * Port to SUBLEQ eForth <https://github.com/howerj/subleq>
\ * Use 32 directories per block?
\ * Allow multiple disks? (store globals in structure)
\ * Hide internal words in a wordlist
\
\ ## Format
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
\ ### Text Directory Format
\
\        \ DIRNAME BLK
\        F FILE.FTH  BLK LEN
\        T FILE.TXT  BLK LEN
\        D DIRECTORY BLK
\
\ Newer directories/files that are more like variable length
\ files will use a different starting word instead of "F"/"T"
\ or "D". "LEN" is the number of bytes used in the last block.
\ "BLK" points to an entry in the FAT.
\

use ffs.fb

wordlist constant {ffs} \ TODO: Add all this to this wordlist

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
: token bl word ;
: nul? count nip 0= ; ( a -- f : is counted word empty? )
: grab ( <word> -- a : get word from input stream  )
  begin token dup nul? 0= ?exit drop query again ;
: integer grab count numberify nip ; ( <num> -- n f : get int )
: integer? integer 0= dpl @ 0>= or -$18 and throw ;
\ : ingest ( a u -- : opposite of 'dump', load nums into mem )
\  cell / for aft integer? over ! cell+ then next drop ;

: -+ drop swap 0< if negate then ;
: nget ( "123" -- : get a single signed number )
  token dup 1+ c@ [char] - = tuck -
  0 0 rot convert drop ( should throw if not number... )
  -+ ;

: line ( k l -- a u )
 [ $6 ] literal lshift swap block + [ $40 ] literal ;
\ : loadline line evaluate ; ( k l -- ??? : execute a line! )


defined ?depth 0= [if]
: ?depth depth 1- > -4 and throw ;
[then]

defined (order) 0= [if]
: (order) ( w wid*n n -- wid*n w n )
  dup if
    1- swap >r recurse over r@ xor
    if 1+ r> -rot exit then rdrop
  then ;
: -order get-order (order) nip set-order ; ( wid -- )
: +order dup >r -order get-order r> swap 1+ set-order ;
[then]

defined b/buf 0= [if] 1024 constant b/buf [endif]

defined editor 0= [if]
wordlist constant {editor}
: editor [ {editor} ] literal +order ; ( BLOCK editor )
{editor} +order definitions
: q [ {editor} ] literal -order ; ( -- : quit editor )
: ? scr @ . ;    ( -- : print block number of current block )
: l scr @ list ; ( -- : list current block )
: x q scr @ load editor ; ( -- : evaluate current block )
: ia 2 ?depth [ $6 ] literal lshift + scr @ block + tib
  >in @ + swap source nip >in @ - cmove tib @ >in ! l ;
: a 0 swap ia ; ( line --, "line" : insert line at )
: w get-order [ {editor} ] literal #1 ( -- : list cmds )
     set-order words set-order ; 
: s update flush ; ( -- : save edited block )
: n  1 scr +! l ; ( -- : display next block )
: p -1 scr +! l ; ( -- : display previous block )
: r scr ! l ; ( k -- : retrieve given block )
: z scr @ block b/buf blank l ; ( -- : erase current block )
: d 1 ?depth >r scr @ block r> [ $6 ] literal lshift +
   [ $40 ] literal blank l ; ( line -- : delete line )
only forth definitions
[endif]

$0000 constant blk.end      \ End of FAT chain
$FFFC constant blk.unused   \ Unmapped / Not memory
$FFFD constant blk.bad-blk  \ Block is bad
$FFFE constant blk.special  \ Special blocks
$FFFF constant blk.free     \ Block is free to use
16 constant maxname
8 constant maxdir
create dirstk maxdir cells allot dirstk maxdir cells erase
create namebuf maxname allot namebuf maxname blank
variable dirp 0 dirp ! \ Directory Pointer

variable version  $0001 version ! \ Version
variable start    1 start !       \ Starting block
0 constant init                   \ Initial program block
1 constant fat                    \ FAT Block
2 constant dirstart               \ Top level directory
16 constant l/blk                 \ Lines per block
64 constant c/blk                 \ Columns per block
variable end      128 end !       \ End block
variable loaded   0 loaded !      \ Loaded initial program?

: block? ( blk -- blk )
  start @ + dup start @ end @ 1+ within 0= -1 and throw ;
: addr? block? block ; ( blk -- addr )
: fat? fat block? ; ( -- fat-blk )
: dirstart? dirstart block? ; ( -- start-dir-blk )

: 16! 2dup c! swap 8 rshift swap 1+ c! update ;
: 16@ dup c@ swap 1+ c@ 8 lshift or ;
: link 2* fat addr? + 16@ ; ( blk -- blk )
: reserve 2* fat addr? + 16! ; ( blk blk -- )
: fmt.init
  init addr? b/buf blank
  s" .( HOWERJ SIMPLE FORTH FILE SYSTEM ) cr 1 loaded ! " 
  init addr? swap cmove update flush ;
: fmt.fat
  fat addr? b/buf erase
  0 b/buf 2/ 1- for blk.unused over reserve 1+ next drop
  blk.special init reserve
  blk.special fat reserve
  blk.special dirstart reserve
  dirstart 1+
  begin
    end @ over >
  while
    blk.free over reserve 1+
  repeat
  drop
  update flush ;
: bblk block b/buf blank update flush ; ( blk -- )
: fblk block b/buf erase update flush ; ( blk -- )
: fblks over - 1- for dup fblk 1+ next drop ; ( lo hi -- )
: fmt.blks dirstart 1+ end @ fblks ;
: fdisk fmt.init fmt.fat fmt.blks ; \ TODO: Format first directory
: free? fat addr? b/buf 2/ 0 ( -- blk : find free block )
  begin
   2dup >
  while
   2 pick over + 16@ blk.free = if nip nip 0 then
   2 +
  repeat 2drop drop 0 -1 ;
\ TODO: Sanity checking (retrieve value and make sure it is
\ not special / invalid / bad block / etcetera
\ TODO: Does one extra
: reserve-range ( blk n -- : allocate range of blocks )
  ?dup 0= if drop exit then
  1- for
    dup 1+ tuck swap reserve
  next
  blk.end swap reserve flush ;
: free-list ( blk -- : free a linked list ) 
  begin
  dup link swap blk.free swap reserve
  dup blk.end = until blk.free swap reserve flush ; 
: link-load ; ( file -- ) \ TODO: Load through FAT
: link-list ; ( file -- ) \ TODO: List through FAT
: append ; ( blk file -- ) \ TODO: Append block to FAT list
\ TODO: Allocate contiguous range
: contiguous ( n -- blk )
  ?dup 0= if -1 exit then
  >r
  0
  begin
    dup < end @
  while
    dup link blk.free = if
      0 begin
        dup r@ <
      while
        2dup + link blk.free <> if drop then
        \ TODO: Reserve blocks if exist in linked list
        \ TODO: Skip entire range
        1+
      repeat
      \ reserve-range, exit
    then
    1+
  repeat rdrop drop -1 ;

: dirp? dirp @ 0 maxdir within 0= if 0 dirp ! -2 throw then ;
: (dir) dirp? dirstk dirp @ cells + ;
: pushd (dir) ! 1 dirp +! ; ( dir -- )
: popd -1 dirp +! (dir) @  ; ( -- dir )
: peekd popd dup pushd ; ( -- dir )
: ls peekd list ;

: (df)
  0 fat addr? b/buf 2/ 1- for
    dup 16@ 3 pick = if 
     swap 1+ swap
    then
    2 +
  next drop nip ;
: df cr
   loaded @ 0= if ." NO DISK" cr exit then
   ." MOUNTED" cr
   ." START BLK: " start @ u. cr
   ." END BLK:   " end @ u. cr
   ." MAX DIRS:  " maxdir u. cr
   ." MAX:       "  end @ start @ - dup . ." / " b/buf * u. cr
   ." FREE:      "  blk.free (df) dup . ." / " b/buf * u. cr ;
: .fat fat addr? b/buf 2/ 1- for dup 16@ . 2 + next drop ;

: nlen? dup maxname > -1 and throw ; ( n -- n )
: nclear namebuf maxname blank ; ( -- )
: ncopy nclear nlen? namebuf swap cmove ; ( c-addr u )
: nname namebuf maxname ; ( -- c-addr u )
: hexp base @ >r hex 0 <# # # # # [char] $ hold #> r> base ! ;

: fmtdirent ( c-addr u dir line blk blk type -- )
;

: fmtdir ( c-addr u dir -- )
  >r ncopy
  s" \ " r@ addr? swap cmove
  nname r@ addr? 2 + swap cmove
  r@ hexp r> addr? 3 + maxname + swap cmove
  update flush ;

: >la dup 0 16 within 0= -1 and throw c/blk * ;
: dir-find ( c-addr u blk -- line | -1 )
  >r ncopy
  1 ( skip first line at zero, this contains directory info )
  begin
    dup l/blk <
  while
    dup >la r@ addr? + nname compare 0= if rdrop exit then
    1+
  repeat
  rdrop drop -1 ; 
: dir-insert ; ( c-addr u blk )
: dir-erase >la swap addr? + c/blk blank ; ( blk line )

: empty? ( blk -- line | -1 : get empty line )
  addr? 0 
  begin
   dup l/blk <
  while
   over c@ bl <= if nip exit then
   swap c/blk + swap 1+ cr
  repeat
  2drop -1 ;

: newdir peekd dup empty? dup 0< -1 and throw ; ( -- dir ) \ find free fat, fmt

\ wordlist constant {shell} \ TODO: Add commands to this wordlist

: mkdir 
  ; \ get string, peekd, find empty+insert, fat, fmt, unique check
: rmdir ; \ get string, peekd, find, erase, compact unlink fat
: cd 
  \ special dirs ".", "..", "/" (full parsing should be done, eventually)
; \ get string, peekd, find, get block, pushd
: exe ; \ get string, peekd, find, get block range, thru
: cat ; \ get string, peekd find, list
: shell ; \ get line / error handling / execute
: tree ; \ recursive tree view
: defrag ; \ compact disk
: help ;

dirstart? pushd

0 block? load
loaded @ 0= [if]
.( Forth File System not present, formatting... ) cr
fdisk
\ mkdir [root]
.( Done ) cr
[then]

