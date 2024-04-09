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
\ * C utility for manipulating disk images / collating
\ files.
\ * Documentation including file system limitations
\ The first block contains executable code and is executed in
\ an attempt to mount the file system.
\ * Use $" instead of s"?
\ * If "BYE" is called within an executed script, this needs
\ ignore, that can be done by redefining it.
\ * Port to SUBLEQ eForth <https://github.com/howerj/subleq>
\ * Use 32 directories per block?
\ * Allow multiple disks? (store globals in structure)
\ * Hide internal words in a wordlist
\ * Unit tests, online help, ...
\ * Optional case insensitivity in file names
\ * Map special files into root directory?
\ * Detect bad blocks with a wrapper around `block`, setting
\ FAT table.
\ * https://stackoverflow.com/questions/4586972/ Fragmentation
\ calculation
\ * More sanity checking could be done, for example we should
\ never be calling `bfree` on a SPECIAL block. There are many
\ places where this checking *could* and *should* be done.
\ * Full path parsing (e.g a/b/c) for all commands.
\ * Store block in a more optimal way instead of as text?
\ * Zero length files
\ * Refactor system, factor words
\ * Find orphaned nodes, data-structure checking, checksums?
\ * Secure erase
\ * Executable and read-only types
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
: s" postpone $" [ ' count ] literal compile, ; immediate
\ "
.( NOT YET IMPLEMENTED IN SUBLEQ EFORTH ) cr
\ bye
: quine source type cr ; ' quine <ok> !
[else]
use ffs.fb
[then]

defined ?depth 0= [if]
: ?depth depth 1- > -4 and throw ;
[then]

defined spaces 0= [if]
: spaces begin ?dup 0> while bl emit 1- repeat ;
[then]

wordlist constant {ffs} \ TODO: Add all this to this wordlist

defined b/buf 0= [if] 1024 constant b/buf [then]
defined d>s 0= [if] : d>s drop ; [then]

$0000 constant blk.end      \ End of FAT chain
$FFFC constant blk.unused   \ Unmapped / Not memory
$FFFD constant blk.bad-blk  \ Block is bad
$FFFE constant blk.special  \ Special blocks
$FFFF constant blk.free     \ Block is free to use
16 constant maxname
8 constant maxdir
create dirstk maxdir cells allot dirstk maxdir cells erase

: enxt dup 1+ swap ; ( n -- n n )

128
enxt constant EUNKN ( unknown error )
enxt constant EIBLK ( bad block )
enxt constant EFILE ( file not found )
enxt constant EFULL ( disk full )
enxt constant EFSCK ( corrupt datastructure / disk )
enxt constant EEXIS ( already exists )
enxt constant EPATH ( path too long )
enxt constant EFLEN ( file length )
enxt constant EDFUL ( directory full )
enxt constant EDNEM ( directory not empty )
enxt constant ENFIL ( not a file )
enxt constant ENDIR ( not a directory )
enxt constant EARGU ( invalid argument )
enxt constant EINTN ( internal error )
drop

: e>s ( code -- )
  dup EUNKN = if drop s" unknown error" exit then
  dup EIBLK = if drop s" bad block" exit then
  dup EFILE = if drop s" file not found" exit then
  dup EFULL = if drop s" disk full" exit then
  dup EFSCK = if drop s" corrupt disk" exit then
  dup EEXIS = if drop s" already exists" exit then
  dup EPATH = if drop s" path too long" exit then
  dup EFLEN = if drop s" file length" exit then
  dup EDFUL = if drop s" directory full" exit then
  dup EDNEM = if drop s" directory not empty" exit then
  dup ENFIL = if drop s" not a file" exit then
  dup ENDIR = if drop s" not a directory" exit then
  dup EARGU = if drop s" invalid argument" exit then
  dup EINTN = if drop s" internal error" exit then
  drop s" unknown" ;

variable error-level 0 error-level !
: elucidate dup error-level ! ?dup if cr e>s type ." ?" then ;
: error swap if dup elucidate throw then drop ; ( f code -- )

: namebuf: create here maxname dup allot blank does> maxname ;

namebuf: namebuf
namebuf: findbuf
create copy-store 1024 allot
variable dirp 0 dirp ! \ Directory Pointer
variable read-only 0 read-only !
variable version  $0100 version ! \ Version

defined eforth [if]
variable start    64 start !      \ Starting block
variable end      126 end !       \ End block
[else]
variable start    1 start !       \ Starting block
variable end      128 end !       \ End block
[then]
0 constant init                   \ Initial program block
1 constant fat                    \ FAT Block
2 constant dirstart               \ Top level directory
16 constant l/blk                 \ Lines per block
64 constant c/blk                 \ Columns per block
16 constant d/blk                 \ Directories per block
variable loaded   0 loaded !      \ Loaded initial program?
variable eline 0 eline !
variable exit-shell 0 exit-shell !

variable file-blk \ pointer to block for a single file
variable file-ptr \ pointer within block

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
: addr? block? block ; ( blk -- addr )
: eline? eline @ ;
: 16! 2dup c! swap 8 rshift swap 1+ c! modify ;
: 16@ dup c@ swap 1+ c@ 8 lshift or ;
: link 2* fat addr? + 16@ ; ( blk -- blk )
: previous ( head-blk prior-to-blk -- blk f )
  swap
  begin
    2dup link = if
      nip
      -1
      exit
    then 
    link
    dup blk.end =
  until 2drop 0 0 ;
: reserve 2* fat addr? + 16! modify ; ( blk blk -- )
: btotal end @ start @ - ;
: bcheck btotal 4 < -1 and throw ;
: fmt.init
  init addr? b/buf blank
  s" .( HOWERJ SIMPLE FORTH FILE SYSTEM ) cr 1 loaded ! " 
  init addr? swap cmove save ;
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
  save ;
: bblk addr? b/buf blank save ; ( blk -- )
: fblk addr? b/buf erase save ; ( blk -- )
: fblks over - 1- for dup fblk 1+ next drop ; ( lo hi -- )
: fmt.blks dirstart end @ fblks ;
: free? ( -- blk f )
  fat addr? 0
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
: bfree ( blk -- : free a linked list ) 
  begin
  dup link swap blk.free swap reserve
  dup blk.end = until save ; 
: bcount ( blk -- n )
  0 swap begin swap 1+ swap link dup blk.end = until drop ;
: reserve-range ( blk n -- : force allocate contiguous blocks )
  begin
    ?dup
  while
    dup 1 <= 
    if over blk.end swap 
    else over dup 1+ swap then reserve 
    1- swap 1+ swap
  repeat drop save ;
: xdump ( blk -- )
  base @ >r hex addr? cr
  c/blk 1- for
    l/blk 1- for
      count 0 <# bl hold # # #> type
    next cr
  next r> base ! drop ;
: link-load ( file -- )
  begin dup >r block? load r> link dup blk.end = until drop ; 
: link-list ( file -- )
  begin dup block? list link dup blk.end = until drop ; 
: link-blank ( file -- )
  begin dup bblk link dup blk.end = until drop ;
: link-xdump ( file -- )
  begin dup block? xdump link dup blk.end = until drop ; 
: more? key [ 32 invert ] literal and [char] Q = ;
: more> cr ." --- (q)uit? --- " ;
: moar block? list more> more? ;
: link-more ( file -- ) 
  begin 
    dup moar if drop exit then link dup blk.end = 
  until drop ; 
: fat-end ( blk -- blk )
  begin dup link blk.end = if exit then link again ;

\ N.B. `fat-append` does not set the appended block to
\ `blk.end`, `balloc` does however. This is so another linked
\ list can be appended. It could set it intelligently 
\ however...
: fat-append fat-end reserve ; ( blk file -- )
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
    r@ +
  repeat rdrop 2drop 0 0 ;
: cballoc ( n -- blk f : allocate contiguous slab )
  dup contiguous if tuck swap reserve-range -1 exit then 0 ;
: dirp? dirp @ 0 maxdir within 0= if 0 dirp ! -2 throw then ;
: (dir) dirp? dirstk dirp @ cells + ;
: pushd (dir) ! 1 dirp +! ; ( dir -- )
: popd dirp @ if -1 dirp +! then (dir) @  ; ( -- dir )
: peekd popd dup pushd ; ( -- dir )

\ TODO: Quantify fragmentation, measure largest alloc block
: df cr
   loaded @ 0= if ." NO DISK" cr exit then
   ." MOUNTED" cr
   ." BLK SZ:    " b/buf u. cr
   ." START BLK: " start @ u. cr
   ." END BLK:   " end @ u. cr
   ." MAX DIRS:  " maxdir u. cr
   ." MAX:       " end @ start @ - dup . ." / " b/buf * u. cr
   ." FREE:      " blk.free btally dup . ." / " b/buf * u. cr ;

: .hex base @ >r hex 0 <# # # # # #> type r> base ! ;
: (.fat)
  b/buf 2/ swap - 1- .hex ." :"
  dup blk.free    = if ." FREE " drop exit then
  dup blk.bad-blk = if ." BADB " drop exit then
  dup blk.end     = if ." END! " drop exit then
  dup blk.special = if ." SPEC " drop exit then
  dup blk.unused  = if ." NUSD " drop exit then
  .hex space ;
: .ffat ( -- )
  cr
  fat addr? b/buf 2/ 1- for 
    dup 16@ r@ (.fat) 2 + 
    r@ b/buf 2/ swap - 8 mod 0= if cr then
  next drop ;
: .sfat fat addr? end @ 2 * dump ; ( -- )
: .fat fat addr? b/buf 2/ 1- for dup 16@ . 2 + next drop ;
: nlen? dup maxname > -1 and throw ; ( n -- n )
: nclear namebuf blank ; ( -- )
: ncopy nclear nlen? namebuf drop swap cmove ; ( c-addr u )
: fclear findbuf blank ; ( -- )
: fcopy fclear nlen? findbuf drop swap cmove ; ( c-addr u )
: hexp base @ >r hex 0 <# # # # # [char] $ hold #> r> base ! ;
: >la dup 0 16 within 0= -1 and throw c/blk * ;
: index >la swap addr? + ;
: dirent-type! index dup >r c! bl r> 1+ c! save ;
: dirent-type@ index c@ ;
: dirent-name! index 2 + swap cmove save ;
: dirent-name@ index 2 + maxname ; 
: dirent-blk@ ( blk line -- n )
  index maxname + 2 + 5 numberify 0= throw d>s ;
: dirent-blk!  ( n blk line -- )
  index maxname + 2 + >r hexp r> swap cmove save ;
: dirent-erase ( blk line )
  >la swap addr? + c/blk blank modify ; 
: >copy addr? copy-store b/buf cmove ;
: copy> addr? copy-store swap b/buf cmove ;

: fmtdir ( c-addr u dir -- )
  dup bblk
  >r fcopy
  [char] \ r@ 0 dirent-type!
  findbuf r@ 0 dirent-name!
  r@ r> 0 dirent-blk! 
  save ;

: dir-find ( c-addr u blk -- line | -1 )
  >r fcopy
  1 ( skip first line at zero, this contains directory info )
  begin
    dup l/blk <
  while
    dup r@ swap dirent-name@ findbuf compare 
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
  addr? c/blk + 1 ( skip first line )
  begin
   dup l/blk <
  while
   over c@ bl <= if nip exit then
   swap c/blk + swap 1+
  repeat
  2drop -1 ;
: is-empty? empty? 1 <> ; ( blk -- f )
: fmt.root 
  s"  [ROOT]" ncopy namebuf dirstart
  fmtdir blk.end dirstart reserve save ;
: /root dirp @ for popd drop next ;
: dir? dirent-type@ [char] D = ; ( dir line -- f )
: (rm) ( f -- )
  namebuf peekd dir-find dup >r 0< EFILE error
  peekd r@ dir? if
    0= ENFIL error
    peekd r@ dirent-blk@ is-empty? EDNEM error
  then
  peekd r@ dirent-blk@ bfree
  peekd r@ dirent-erase
  peekd addr? r@ c/blk * + dup c/blk + swap b/buf r@ 1+ c/blk * 
  - cmove
  save rdrop drop ;

: (copy) ( src-blks dst-blks )
  swap
  begin
    dup >copy over copy>
    link swap link swap
    dup
    blk.end =
  until ( <> throw ) 2drop ;

\ TODO: Modify editor to work with FAT table, or make a new
\ one to work with it.
wordlist constant {editor}
variable vista 1 vista ! \ Used to be `scr`
defined editor 0= [if]
: editor [ {editor} ] literal +order ; ( BLOCK editor )
{editor} +order definitions
: q [ {editor} ] literal -order ; ( -- : quit editor )
: ? vista @ . ;    ( -- : print block number of current block )
: l vista @ list ; ( -- : list current block )
: x q vista @ load editor ; ( -- : evaluate current block )
: ia 2 ?depth [ $6 ] literal lshift + vista @ block + tib
  >in @ + swap source nip >in @ - cmove tib @ >in ! l ;
: a 0 swap ia ; : i a ; ( line --, "line" : insert line at )
: w get-order [ {editor} ] literal #1 ( -- : list cmds )
     set-order words set-order ; 
: s save ; ( -- : save edited block )
: n  1 vista +! l ; ( -- : display next block )
: p -1 vista +! l ; ( -- : display previous block )
: r vista ! l ; ( k -- : retrieve given block )
: z vista @ block b/buf blank l ; ( -- : erase current block )
: d 1 ?depth >r vista @ block r> [ $6 ] literal lshift +
   [ $40 ] literal blank l ; ( line -- : delete line )
only forth definitions
[then]

: .dir ( blk -- )
  cr
  ( ." /" dup 0 dirent-name@ type cr )
  1
  begin
    dup l/blk <
  while
    2dup dirent-type@ bl <= if 2drop exit then
    2dup dirent-type@ [char] D = if ." DIR " else ." FILE" then
    2 spaces
    2dup dirent-name@ type space
    2 spaces
    2dup dirent-blk@ bcount u.
    cr
    1+
  repeat 2drop ;

: narg token count ncopy ; ( "token" -- )
: (file) ( "file" -- blk )
  narg
  namebuf peekd dir-find dup 0< EFILE error
  peekd over dirent-type@ [char] D = ENFIL error
  peekd swap dirent-blk@ ;

: found? peekd eline? ; ( -- cwd line )
: cmd> cr ." CMD> " ;
: (shell) \ TODO: Implement
  grab count 2dup ncopy
  2dup peekd dir-find dup >r 0< if
    rdrop evaluate
  else
    2drop ." FILE EXISTS"
    rdrop
  then
  \ TODO: Find file, execute? Path var? Execute command
;

: full? ( -- is cwd full? )
  peekd empty? dup eline ! -1 = EDFUL error ;

\ wordlist constant {shell}
\ TODO: Use this
\ {shell} +order definitions

: halt save 1 exit-shell !  ;
: ls peekd .dir ; 
: dir peekd block? list ;
: mount init block? load 0 dirp ! dirstart pushd ;
: fdisk bcheck fmt.init fmt.fat fmt.blks fmt.root mount ; 
: rename ( "file" "file" -- )
  narg
  namebuf peekd dir-find dup >r 0< EFILE error
  narg ( dir-find uses `findbuf` )
  namebuf peekd dir-find 0>= EEXIS error
  findbuf peekd r> dirent-name! ;
: mkdir ( "dir" -- )
  full?
  narg
  namebuf peekd dir-find 0>= EEXIS error
  namebuf found? dirent-name!
  balloc dup >r found? dirent-blk!
  [char] D found? dirent-type!
  namebuf r> fmtdir ; 
: copy ( "src" "dst" -- )
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
: mkfile ( "file" -- )
  full?
  narg
  namebuf peekd dir-find 0>= EEXIS error
  namebuf found? dirent-name!
  balloc dup bblk found? dirent-blk!
  [char] F found? dirent-type! ;
: fallocate ( "file" count -- )
  full?
  narg
  integer? >r
  namebuf peekd dir-find 0>= EEXIS error
  namebuf found? dirent-name!
  r> ballocs dup link-blank found? dirent-blk!
  [char] F found? dirent-type!  ;
: rm narg 0 (rm) ; ( "file" -- )
: rmdir narg 1 (rm) ; ( "dir" -- )
: cd  ( "dir" -- )
  token count 2dup s" ." compare 0= if 2drop exit then
  2dup s" .." compare 0= if 2drop popd drop exit then
  2dup s" /" compare 0= if 2drop /root exit then
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
  repeat
  drop ;

: shell \ get line / error handling / execute
( only {shell} +order )
  begin
    cmd>
    [ ' (shell) ] literal catch elucidate exit-shell @ 
  until 0 exit-shell ! ; 
: tree ; \ recursive tree view
: deltree ; \ recursive delete
: stat ;
: defrag ; \ compact disk
: chkdsk ;
: grep ; ( search file -- )

: help ;

: cat (file) link-list ; ( "file" -- )
: more (file) link-more ; ( "file" -- )
: exe (file) link-load  ; ( "file" -- )
: hexdump (file) link-xdump ; ( "file" -- )

{editor} +order
: edit (file) block? vista ! editor ; ( "file" -- )
{editor} -order


\ Aliases
: cls page ;
: cp copy ;
: touch mkfile ;
: chdir cd ;
: del rm ;
: sh shell ;
\ : erase rm ;
\ : bye halt ;
\ : exit halt ;
\ : quit halt ;

defined eforth [if] .( HERE: ) here u. cr [then]

mount loaded @ 0= [if]
cr .( FFS NOT PRESENT, FORMATTING... ) cr
fdisk
.( DONE ) cr
[then]

