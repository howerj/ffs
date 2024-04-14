\ # Simple Forth File System
\
\ * Author: Richard James Howe
\ * License: Public Domain / The Unlicense
\ * Repo: <https://github.com/howerj/ffs>
\ * Email: <mailto:howe.r.j.89@gmail.com>
\
\ ## TO DO
\ 
\ * Rewrite `list` so we have more control over output?
\ * Shell loop that handles custom error
\ * C utility for manipulating disk images / collating
\ files.
\ * Documentation including file system limitations
\ The first block contains executable code and is executed in
\ an attempt to mount the file system.
\ * Optional case insensitivity in file names
\ * https://stackoverflow.com/questions/4586972/ Fragmentation
\ calculation
\ * Full path parsing (e.g a/b/c) for all commands.
\ * Zero length files, store `blk.end` in block dirent
\ * Refactor system, factor words
\ * Find orphaned nodes, data-structure checking, checksums?
\ * Secure erase
\ * Executable and read-only types
\ * For SUBLEQ eFORTH we could mark the first 64 blocks as 
\ being special, as well as the last block, and not use
\ offsets. This would allow the file system direct access to
\ the SUBLEQ code, including tasks. This could even be present
\ in the file system as a special file, with a linked entry in
\ the FAT.
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
\ The directory format is incredibly simple, it consists of
\ fixed width text fields that are human and machine readable.
\ A directory fits into a single block, which puts an upper
\ limit on a directory size (without extensions that look up
\ the next entry in the FAT table).
\
\        \ DIRNAME   BLK
\        F FILE.FTH  BLK
\        F FILE.TXT  BLK
\        D DIRECTORY BLK
\        S FILE.SPC  BLK
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
\ ### Block Editor
\
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

wordlist constant {dos}
: dos only {dos} +order ;
wordlist constant {ffs} 
{ffs} +order definitions

defined b/buf 0= [if] 1024 constant b/buf [then]
defined d>s 0= [if] : d>s drop ; [then]

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
create dirstk maxdir cells allot dirstk maxdir cells erase

: namebuf: create here maxname dup allot blank does> maxname ;
namebuf: namebuf
namebuf: findbuf
namebuf: compbuf
create copy-store 1024 allot
variable dirp 0 dirp ! \ Directory Pointer
variable read-only 0 read-only !  \ Make file system read only 
variable version  $0100 version ! \ Version

defined eforth [if]
variable start 65 start !      \ Starting block
variable end   126 end !       \ End block
[else]
variable start 1 start !       \ Starting block
variable end   128 end !       \ End block
[then]
2 constant dsl                 \ Directory Start Line
0 constant init                \ Initial program block
1 constant fat                 \ FAT Block
2 constant dirstart            \ Top level directory
16 constant l/blk              \ Lines per block
64 constant c/blk              \ Columns per block
32 constant d/blk              \ Directories per block
32 constant dirsz              \ Length of directory entry
variable loaded   0 loaded !   \ Loaded initial program?
variable eline 0 eline !       \ Empty link in directory
variable exit-shell 0 exit-shell !

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
: btotal end @ start @ - ; ( -- n )
: bcheck btotal 4 < -1 and throw ;
: fmt.init
  init addr? b/buf blank
  s" .( HOWERJ SIMPLE FORTH FILE SYSTEM ) cr 1 loaded ! " 
  init addr? swap cmove save ;
: fmt.fat
  fat addr? b/buf erase
  0 b/buf 2/ 1- for blk.unmapped over reserve 1+ next drop
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
: bblk addr? b/buf blank save ; ( blk -- )
: fblk addr? b/buf erase save ; ( blk -- )
: fmt.blks
  dirstart end @ dirstart - start @ - 1- for
    dup fblk 1+
  next drop ;
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
\ TODO: Link in ascending order
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
  dup blk.end = until save ; 
: bcount ( blk -- n )
  0 swap begin swap 1+ swap link dup blk.end = until drop ;
: btruncate ( n blk -- )
  -1 throw \ TODO Get this working
  dup >r bcount 1 over 1+ within 0= if drop exit then
  r> swap
  begin
    ?dup
  while
    swap link swap
    1-
  repeat dup bfree blk.end swap reserve ;
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
: apply ( file xt -- : apply execution token to file )
  >r begin dup r@ swap >r execute r> link dup blk.end = until 
  rdrop drop ;
: +list block? list ; ( blk -- )
: +load block? load ; ( blk -- )
: link-load [ ' +load ] literal apply ; ( file -- )
: link-list [ ' +list ] literal apply ; ( file -- )
: link-blank [ ' bblk ] literal apply ; ( file -- )
: link-xdump [ ' xdump ] literal apply ; ( file -- )
: link-u [ ' u. ] literal apply ; ( file -- )
: more? key [ 32 invert ] literal and [char] Q = ;
: more> cr ." --- (q)uit? --- " ;
: moar +list more> more? ;
: link-more ( file -- ) 
  begin 
    dup moar if cr ." QUIT" drop exit then link dup blk.end = 
  until drop cr ." EOF" ; 
: fat-end ( blk -- blk : last block in FAT chain )
  begin dup link blk.end = if exit then link again ;

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
: .hex base @ >r hex 0 <# # # # # #> type r> base ! ;
: hexp base @ >r hex 0 <# # # # # [char] $ hold #> r> base ! ;
: cvalid ( ch -- f : is character valid for a dir name? )
  dup 47 = if drop 0 exit then
  32 127 within ;
: nvalid? ( c-addr u -- f )
  2dup s" ." ccopy compbuf compare 0= if 2drop 0 exit then
  2dup s" .." ccopy compbuf compare 0= if 2drop 0 exit then
  begin
   ?dup
  while
   over c@ cvalid 0= if 2drop 0 exit then
   swap 1+ swap 1-
  repeat drop -1 ;
: (.fat)
  b/buf 2/ swap - 1- .hex ." :"
  dup blk.free     = if ." FREE " drop exit then
  dup blk.bad-blk  = if ." BADB " drop exit then
  dup blk.end      = if ." END! " drop exit then
  dup blk.special  = if ." SPEC " drop exit then
  dup blk.unmapped = if ." NMAP " drop exit then
  .hex space ;
: .ffat ( -- )
  cr
  fat addr? b/buf 2/ 1- for 
    dup 16@ r@ (.fat) 2 + 
    r@ b/buf 2/ swap - 8 mod 0= if cr then
  next drop ;
: .sfat fat addr? end @ 2 * dump ; ( -- )
: .fat fat addr? b/buf 2/ 1- for dup 16@ . 2 + next drop ;

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
: dirent-erase ( blk line )
  >la swap addr? + dirsz blank modify ; 
: >copy addr? copy-store b/buf cmove ;
: copy> addr? copy-store swap b/buf cmove ;

: fmtdir ( c-addr u dir -- )
  dup bblk
  >r fcopy
  [char] \ r@ 0 dirent-type!
  findbuf r@ 0 dirent-name!
  r@ r> 0 dirent-blk! 
  save ;

\ defined aft 0= [if]
\ : aft  ( -- aft for gforth )
\    2drop drop
\    postpone branch >mark
\    postpone begin drop do-dest
\    postpone but ; immediate
\ [then]
\ 
\ \ TODO: Get working in gforth (cannot call `exit` in
\ \ FOR...NEXT loop in gforth... 
\ 
\ : >upper dup 97 123 within if 32 xor then ;
\ : icompare ( a1 u1 a2 u2 -- n : string comparison )
\   rot
\   over - ?dup if >r 2drop r> nip exit then
\   for ( a1 a2 )
\     aft
\       count >upper rot count >upper rot - ?dup
\       if rdrop nip nip exit then
\     then
\   next 2drop 0 ;
 
: dir-find ( c-addr u blk -- line | -1 )
  >r fcopy
  dsl ( skip first line at zero, this contains directory info )
  begin
    dup d/blk <
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
  addr? dirsz dsl * + dsl ( skip first line )
  begin
   dup d/blk <
  while
   over c@ bl <= if nip exit then
   swap dirsz + swap 1+
  repeat
  2drop -1 ;
: is-empty? empty? dsl <> ; ( blk -- f )
: fmt.root 
  ( s" [ROOT]" ncopy namebuf dirstart )
  nclear namebuf dirstart
  fmtdir blk.end dirstart reserve save ;
: /root dirp @ for popd drop next ;
: dir? dirent-type@ [char] D = ; ( dir line -- f )
: special? dirent-type@ [char] S = ; ( dir line -- f)
: file? dirent-type@ [char] F = ; ( dir line -- f)

\ TODO: Get working
\ : (remove) ( dir line f -- )
\   >r 2dup dir? if
\     r@ 0= ENFIL error
\     2dup dirent-blk@ is-empty? EDNEM error
\   then
\   rdrop
\   2dup special? 0= if 2dup dirent-blk@ bfree then
\   2dup dirent-erase
\   >r addr? r@
\   dirsz * + dup dirsz + swap b/buf r@ 1+ dirsz * 
\   - cmove
\   rdrop save drop ;
\ : (rm) ( f --, call narg before )
\  >r namebuf peekd dir-find dup 0< EFILE error
\  peekd swap r> (remove) ;

: (rm) ( f --, call narg before )
  namebuf peekd dir-find dup >r 0< EFILE error
  peekd r@ dir? if
    0= ENFIL error
    peekd r@ dirent-blk@ is-empty? EDNEM error
  then
  peekd r@ special? 0= if peekd r@ dirent-blk@ bfree then
  peekd r@ dirent-erase
  peekd addr? r@ dirsz * + dup dirsz + swap b/buf r@ 1+ dirsz * 
  - cmove
  save rdrop drop ;

: (copy) ( src-blks dst-blks )
  swap
  begin
    dup >copy over copy>
    link swap link swap
    dup
    blk.end =
  until <> throw ;

variable vista 1 vista ! \ Used to be `scr`
variable head 1 head !
variable line 0 line !
wordlist constant {edlin}
: edlin ( BLOCK editor )
  vista ! head ! 0 line ! [ {edlin} ] literal +order ; 
{edlin} +order definitions
\ TODO: Command to delete block from linked list
: s save ; ( -- : save edited block )
: q s [ {edlin} ] literal -order ; ( -- : quit editor )
: ? vista @ . ;    ( -- : print block number of current block )
: l vista @ block? list ; ( -- : list current block )
: x q head @ link-load edlin ; ( -- : exe file )
: ia 2 ?depth [ $6 ] literal lshift + vista @ addr? + tib
  >in @ + swap source nip >in @ - cmove tib @ >in ! l ;
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
: z vista @ addr? b/buf blank l ; ( -- : erase current block )
: d 1 ?depth >r vista @ addr? r> [ $6 ] literal lshift +
   [ $40 ] literal blank l ; ( line -- : delete line )
: - line @ -1 line +! line @ 0< if 0 line ! p then ;
: + line @ a  1 line +! line @ l/blk >= if 0 line ! n then ;
{edlin} -order {ffs} +order definitions

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
      2dup dirent-blk@ bcount u.
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

\ : cmd> cr ." CMD> " ;
\ : (shell) \ TODO: Implement
\  grab count 2dup ncopy
\  2dup peekd dir-find dup >r 0< if
\    rdrop evaluate
\  else
\    2drop ." FILE EXISTS"
\    rdrop
\  then
\  \ TODO: Find file, execute? Path var? Execute command
\ ;

: full? ( -- is cwd full? )
  peekd empty? dup eline ! -1 = EDFUL error ;
: (create) ( -- blk: call narg prior, create or open existing )
  namebuf peekd dir-find dup 
  0>= if peekd swap dirent-blk@ exit then
  drop
  full?
  namebuf found? dirent-name!
  balloc dup link-blank found? dirent-blk!
  [char] F found? dirent-type!
  found? dirent-blk@ ;
: (mkfile) ( n -- : `narg` should have name in it )
  >r
  namebuf peekd dir-find 0>= EEXIS error
  namebuf found? dirent-name!
  r> ballocs dup link-blank found? dirent-blk!
  [char] F found? dirent-type! ;

{dos} +order definitions

: df cr
   loaded @ 0= if ." NO DISK" cr exit then
   ." MOUNTED" cr
   ." BLK SZ:    " b/buf u. cr
   ." START BLK: " start @ u. cr
   ." END BLK:   " end @ u. cr
   ." MAX DIRS:  " maxdir u. cr
   ." MAX:       " end @ start @ - dup . ." / " b/buf * u. cr
   ." FREE:      " blk.free btally dup . ." / " b/buf * u. cr 
   ." BAD BLKS:  " blk.bad-blk btally u. cr 
   ." LARGEST CONTIGUOUS BLOCK: " largest u. cr ;

: fsync save-buffers ;
: halt save 1 exit-shell ! only forth ;
: ls peekd .dir ; 
: dir peekd block? list ;
: mount init block? load 0 dirp ! dirstart pushd ;
: fdisk bcheck fmt.init fmt.fat fmt.blks fmt.root mount ; 
\ TODO: `move`, allow moving into subdirectories or parent dirs
: rename ( "file" "file" -- )
  narg
  namebuf peekd dir-find dup >r 0< EFILE error
  narg ( dir-find uses `findbuf` )
  namebuf peekd dir-find 0>= EEXIS error
  findbuf peekd r> dirent-name! ;
\ : move ( "file" "file" -- )
\  \ TODO Check for ".." and "."
\   narg
\   namebuf peekd dir-find dup >r 0< EFILE error
\   narg peekd dir-find dup >r 0< if \ move?
\     peekd r@ dirent-type@ [char] D <> ENDIR error
\     peekd r@ dirent-blk@
\     
\   else ( rename )
\     findbuf peekd r> dirent-name!
\   then
\  ;
: mkdir ( "dir" -- )
  dirp @ maxdir >= EDDPT error
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
: mkfile full? narg 1 (mkfile) ; ( "file" -- )
: fallocate full? narg integer? (mkfile) ; ( "file" count -- )
: fgrow ( "file" count -- )
  narg integer? 
  namebuf peekd dir-find dup >r 0< ENFIL error
  ballocs dup link-blank peekd r> dirent-blk@ fat-append save ;
: ftruncate ( "file" count -- )
  narg integer?
  namebuf peekd dir-find dup >r 0< ENFIL error
  peekd r> dirent-blk@ btruncate ;
: mknod ( "file" node -- )
  full?
  narg
  integer? >r
  namebuf peekd dir-find 0>= EEXIS error
  namebuf found? dirent-name!
  r> found? dirent-blk! 
  [char] S found? dirent-type! ;
: rm narg 0 (rm) ; ( "file" -- )
: rmdir narg 1 (rm) ; ( "dir" -- )
: cd ( "dir" -- )
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
\ : (rm) ( f -- )
\   namebuf peekd dir-find dup >r 0< EFILE error
\   peekd r@ dir? if
\     0= ENFIL error
\     peekd r@ dirent-blk@ is-empty? EDNEM error
\   then
\   peekd r@ special? 0= if peekd r@ dirent-blk@ bfree then
\   peekd r@ dirent-erase
\ peekd addr? r@ dirsz * + dup dirsz + swap b/buf r@ 1+ dirsz * 
\   - cmove
\   save rdrop drop ;
\ 
\ : (deltree)
\    dsl
\    begin
\      dup d/blk <
\    while
\      peekd over dirent-type@ bl <= if drop exit then
\      peekd over dirent-type@ [char] D = if
\        peekd over dirent-blk@ pushd recurse popd \ delete
\      else
\        
\        \ todo delete file
\      then
\      1+
\    repeat drop ; 
\ : deltree 
\   narg 
\ ;
\ : shell \ get line / error handling / execute
\ ( only {shell} +order )
\  begin
\    cmd>
\    [ ' (shell) ] literal catch elucidate exit-shell @ 
\  until 0 exit-shell ! ; 
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

: defrag ; \ compact disk
: chkdsk ;
: grep ; ( search file -- )
: cmp ;

{edlin} +order
: edit narg (create) dup edlin ; ( "file" -- )
{edlin} -order

\ Aliases
: cls page ;
: cp copy ;
: touch mkfile ;
: chdir cd ;
: del rm ;
: sh exe ;
: ed edit ;
: bye halt ;
: exit halt ;
: quit halt ;
: type cat ;

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
mknod [BOOT] 0
mknod [FAT] 1
cd ..
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
+ Commands:
+
+ cat / type <FILE>: display a file
+ cd / chdir <DIR>: change working directory to <DIR>
+ cls: clear screen
+ cp / copy <FILE1> <FILE2>: copy <FILE1> to new <FILE2>
+ df: display file system information
+ ed / edit <FILE>: edit a <FILE> with the block editor
+ exe / sh <FILE>: execute source <FILE>
+ fallocate <FILE> <NUM>: make <FILE> with <NUM> blocks
+ fdisk: **WARNING** formats disk deleting all data!
+ fgrow <FILE> <NUM>: grow <FILE> by <NUM> blocks
+ fsync: save any block changes
+ halt / quit / bye: safely halt system
+ hexdump <FILE>: hexdump a file
+ ls / dir : list directory
+ mkdir <DIR>: make a directory
+ mknode <FILE> <NUM>: make a special <FILE> with <NUM>
+ more <FILE>: display a file, pause for each block
+ mount: attempt file system mounting
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
+ w : list command
+ #line a / i <LINE>: insert <LINE> onto #line of current block
+ #line #row ia <LINE>: insert <LINE> into #line at #row
+ ? : display current block
+ z : blank current block
+ y : yank block to storage buffer
+ u : replace screen with storage buffer
q
edit demo.fth
+ .( HELLO, WORLD ) cr
+ 2 2 + . cr
q
mkdir home
mkdir bin

.( DONE ) cr
[then]


