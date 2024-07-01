\ LZSS Data Compression
\
\ * 1989-04-06 Standard C by Haruhiko Okumura 
\ * 1994-12-09 Standard Forth by Wil Baden 
\ * 2024-06-18 Porting to SUBLEQ eForth by Richard James Howe
\
\     Use, distribute, and modify this program freely.
\
\ This code is very Un-Forth like and could do with a rewrite,
\ the compression and decompression routines could be factored
\ into smaller words, for example. As the code is working
\ little would be gained from doing so however.
\
\ The code in this module uses globals, a global buffer and
\ and globals for the file handles (`infile` and `outfile`
\ which need to be set correctly before calling the encode
\ or decode functions, and closed afterwards). This simplifies
\ the code but like most Forth code limits its usage.
\


\ TODO: Port to SUBLEQ eForth (1024 buffer, do...loop, ... )
\ TODO: Refactor and factor

defined chars 0= [if] : chars ; immediate [then]
defined recreate-file 0= [if] 
 : recreate-file create-file ; 
[then]

: array   create cells allot does> swap cells + ;
: carray  create chars allot does> swap chars + ;
: checked throw ;

variable infile  0 infile !   \ Input file handle
variable outfile 0 outfile !  \ Output file handle

defined +ffs [if]
: read-char infile @ key-file ;
[else]
: read-char 
  infile @ file-eof? ?dup ?exit
  infile @ key-file ; ( -- char  ) 
[then]

\ TODO: Rename or use LZSS vocab
4096 constant n         \ Size of Ring Buffer
18   constant f         \ Upper Limit for match-length
2    constant threshold \ Encode string into position & length
                        \ if match-length is greater.
n    constant null      \ Index for Binary Search Tree Root

variable textsize    \ text size counter
variable codesize    \ code size counter 

\ These are set by insert-node procedure.
variable match-position
variable match-length

\ Ring buffer of size N, with extra F-1 bytes to facilitate 
\ string comparison.
n f + 1- carray text-buf 

\ Left & Right Children and Parents -- Binary Search Trees
n 1+    array lson
n 257 + array rson
n 1+    array dad

\ For i = 0 to N - 1, rson[i] and lson[i] will be the right and
\ left children of node i. These nodes need not be initialized.
\ Also, dad[i] is the parent of node i. These are initialized 
\ to `null = n`, which stands for `not used.'
\
\ For i = 0 to 255, rson[N + i + 1] is the root of the tree
\ for strings that begin with character i.  These are 
\ initialized to `null`.  Note there are 256 trees. 

: init-tree ( -- Initialize trees )
  n 257 + n 1+ do null i rson ! loop
  n   0        do null i  dad ! loop ;

\ Insert string of length F, text_buf[r..r+F-1], into one of 
\ the trees of text_buf[r]'th tree and return the longest-match 
\ position and length via the global variables match-position 
\ and match-length. If match-length = F, then remove the old 
\ node in favor of the new one, because the old one will be 
\ deleted sooner. Note r plays double role, as tree node and 
\ position in buffer.

: dad? dad @ ;
: lson? lson @ ;
: rson? rson @ ;

: insert-node ( r --  )
  null over lson ! null over rson ! 0 match-length !
  dup text-buf c@ n + 1+ ( r p )
  1                 ( r p cmp )
  begin             ( r p cmp )
    0>= if           ( r p )
      dup rson? null <> if
        rson?
      else
        2dup rson !
        swap dad !      (  )
        exit
      then
    else            ( r p )
      dup lson? null <> if
        lson?
      else
        2dup lson !
        swap dad !      (  )
        exit
      then
    then                        ( r p )
    0 f dup 1 do                ( r p 0 f )
      3 pick i + text-buf c@    ( r p 0 f c )
      3 pick i + text-buf c@ -  ( r p 0 f diff )
      ?dup if
        nip nip i
        leave
      then          ( r p 0 f )
    loop            ( r p cmp i )
    dup match-length @ > if
      2 pick match-position !
      dup match-length !
      f >=
    else
      drop 0
    then            ( r p cmp flag )
  until             ( r p cmp )
  drop              ( r p )
  2dup dad? swap dad !
  2dup lson? swap lson !
  2dup rson? swap rson !
  2dup lson? dad !
  2dup rson? dad !
  dup dad? rson? over = if
    tuck dad? rson !
  else
    tuck dad? lson !
  then              ( p )
  dad null swap ! ; ( remove p  )

: delete-node ( p -- Deletes node p from tree.  )
  dup dad? null = if drop exit then ( not in tree )
  ( case  )              ( p  )
    dup rson? null =
  if
    dup lson?
  else
    dup lson? null =
  if
    dup rson?
  else
    dup lson?          ( p q  )
    dup rson? null <> if
      begin
        rson?
        dup rson? null =
      until
      dup lson? over dad? rson !
      dup dad? over lson? dad !
      over lson? over lson !
      over lson? dad over swap !
    then
    over rson? over rson !
    over rson? dad over swap !
  then then        ( p q  )
  over dad? over dad !
  over dup dad? rson? = if
    over dad? rson !
  else
    over dad? lson !
  then              ( p )
  dad null swap ! ; (  )

17 carray code-buf
variable len
variable last-match-length
variable code-buf-ptr
variable mask

: wrap 1+ [ n 1- ] literal and ;
: bit 1 and ;
: nibble 15 and ;

: lzss-encode ( -- : LZSS File Compression )
  0 textsize ! 0 codesize !
  init-tree

  \ code_buf[1..16] saves eight units of code, and code_buf[0]
  \ works as eight flags, "1" representing that the unit is an
  \ unencoded letter in 1 byte, "0" a position-and-length pair
  \ in 2 bytes.  thus, eight units require at most 16 bytes
  \ of code.
  0 0 code-buf c!
  1 mask c! 1 code-buf-ptr !
  0 [ n f - ] literal           ( s r )
  \ clear the buffer with any character that will appear often
  0 text-buf [ n f - ] literal  bl fill
  \ read f bytes into the last f bytes of the buffer.
  dup text-buf f infile @ read-file checked   ( s r count )
  dup len ! dup textsize !
  0= ?exit         ( s r )

  \ Insert the f strings, each of which begins with one or more
  \ `space' characters.  note the order in which these strings
  \ are inserted.  this way, degenerate trees will be less
  \ likely to occur.  )

  f 1+ 1 do ( s r )
    dup i - insert-node
  loop

  \ Finally, insert the whole string just read. The global 
  \ variables match-length and match-position are set.

  dup insert-node
  begin             ( s r )
    \ match_length may be spuriously long at end of text.
    match-length @ len @ > if len @ match-length ! then
    match-length @ threshold <= if
      \ not long enough match. Send one byte.
      1 match-length !
      \ `send one byte' flag
      mask c@ 0 code-buf c@ or 0 code-buf c!
      \ send uncoded
      dup text-buf c@ code-buf-ptr @ code-buf c!
      1 code-buf-ptr +!
    else
      \ send position and length pair.
      \ note match-length > threshold.
      match-position @  code-buf-ptr @ code-buf c!
      1 code-buf-ptr +!
      match-position @ 8 rshift 4 lshift ( . . j )
        match-length @ threshold - 1- or
        code-buf-ptr @ code-buf c!  ( . . )
      1 code-buf-ptr +!
    then
    \ shift mask left one bit.  )    ( . . )
    mask c@  2*  mask c!    mask c@ 0= if
      \ send at most 8 units of code together
      0 code-buf  code-buf-ptr @     ( . . a k )
        outfile @ write-file checked ( . . )
      code-buf-ptr @  codesize  +!
      0 0 code-buf c! 1 code-buf-ptr ! 1 mask c!
    then ( s r )
    match-length @ last-match-length !
    last-match-length @ dup 0 do  ( s r n )
      read-char          ( s r n c )
      dup 0< if 2drop i leave then
      \ delete old strings and read new bytes.
      3 pick delete-node
      dup 3 1+ pick text-buf c!
      \ if the position is near end of buffer, extend
      \ the buffer to make string comparison easier
      3 pick f 1- < if     ( s r n c )
        dup 3 1+ pick n + text-buf c!
      then
      drop          ( s r n )
      \ since this is a ring buffer, increment the
      \ position modulo n.
      >r >r         ( s )
        wrap
      r>            ( s r )
        wrap
      r>            ( s r n )
      ( register the string in text_buf[r..r+f-1].  )
      over insert-node
    loop            ( s r i )
    dup textsize +!
    \ after the end of text, no need to read, but
    \ buffer may not be empty
    last-match-length @ swap ?do    ( s r )
      over delete-node
      >r  wrap  r>
      wrap
      -1 len +!    len @ if
        dup insert-node
      then
    loop
    len @ 0<=
  until 2drop
  \ send remaining code
  code-buf-ptr @ 1 > if
    0 code-buf  code-buf-ptr @ outfile @ write-file checked
    code-buf-ptr @ codesize +!
  then ;

: lzss-decode ( -- : LZSS decode, reverse of encode )
  0 text-buf [ n f - ] literal bl fill
  0 [ n f - ] literal ( flags r )
  begin
    >r ( flags )
      1 rshift dup 256 and 0= if drop (  )
        read-char            ( c )
        dup 0< if r> 2drop exit then  ( c )
        $FF00 or ( flags  )
        \ uses higher byte to count eight
      then
    r>              ( flags r )
    over bit if
      read-char                      ( . . c )
      dup 0< if drop 2drop exit then ( . r c )
      over text-buf c!               ( . r )
      dup text-buf 1 outfile @ write-file checked
      wrap
    else
      read-char                       ( . . i )
      dup 0< if drop 2drop exit then  ( . r c )
      read-char                       ( . . i j )
      dup 0< if 2drop 2drop exit then ( . . i j )
      dup >r 4 rshift 8 lshift or r>
      nibble threshold + 1+
      0 ?do ( . r i  )
        dup i + n 1- and text-buf ( . r i a )
        dup 1 outfile @ write-file checked
        c@  2 pick text-buf c!  ( . r i )
        >r wrap r>
      loop ( . r i )
      drop ( flags r )
    then
  again ;

: statistics ( -- )
  ." in : " textsize ? cr
  ." out: " codesize ? cr
  textsize @ if
    ." saved: " textsize @ codesize @ - 100 textsize @ */
      2 .r ." %" cr
  then ;

: lzss-file outfile ! infile ! lzss-encode ;
: unlzss-file outfile ! infile ! lzss-decode ;
  
: lzss-file-name ( "from" "to" -- ior )
  w/o recreate-file ?dup if nip exit then outfile !
  r/o open-file ?dup if nip outfile @ close-file drop exit then
  infile !
  [ ' lzss-encode ] literal catch
  infile @ close-file 0 infile !
  outfile @ close-file 0 outfile !
  ?dup if nip nip exit then
  ?dup if nip exit then ;

: unlzss-file-name ( "from" "to" -- ior )
  w/o recreate-file ?dup if nip exit then outfile !
  r/o open-file ?dup if nip infile @ close-file drop exit then
  infile !
  [ ' lzss-decode ] literal catch
  infile @ close-file 0 infile !
  outfile @ close-file 0 outfile !
  ?dup if nip nip exit then
  ?dup if nip exit then ;


defined +ffs [if]
\ Forth File System <https://github.com/howerj/ffs> extensions.
+ffs
: lzss ( "from" "to" -- )
  locked!? ro? 
  narg mcopy narg movebuf namebuf lzss-file-name ;
: unlzss ( "from" "to" -- ) 
  locked!? ro? 
  narg mcopy narg movebuf namebuf unlzss-file-name ;

s" help.txt"  s" help.lzss" lzss-file-name throw statistics
s" help.lzss" s" help.orig" unlzss-file-name throw statistics
[else]
\ If `+ffs` is not defined we are most likely running gforth
\ with the File Access Methods that talk to the normal systems
\ file system, the file `lzss.fth` and `ffs.fth` should be
\ present instead.

s" lzss.fth"  s" lzss.lzss" lzss-file-name throw statistics
s" lzss.lzss" s" lzss.orig" unlzss-file-name throw statistics

s" ffs.fth"  s" ffs.lzss" lzss-file-name throw statistics
s" ffs.lzss" s" ffs.orig" unlzss-file-name throw statistics

[then]
