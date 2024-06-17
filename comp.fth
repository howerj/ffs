\ LZSS Data Compression, Standard Forth -- HO & WB -- 94-12-09
\ Modifications: Richard James Howe, 2024.
\ TODO: Port to SUBLEQ eForth (1024 buffer, do...loop, ... )

\ MARKER      LZSS-Data-Compression


: array    create  cells allot     does>   swap cells + ;
: carray   create  chars allot     does>   swap chars + ;
: checked  abort" file access error. " ;

create single-char-i/o-buffer    0 c,    align
: read-char        ( file -- char  ) 
  single-char-i/o-buffer 1 rot read-file checked if
    single-char-i/o-buffer c@
  else -1 then ;

(     LZSS -- A Data Compression Program  )
(     89-04-06 Standard C by Haruhiko Okumura  )
(     94-12-09 Standard Forth by Wil Baden  )

(     Use, distribute, and modify this program freely.  )

4096  constant    N     ( Size of Ring Buffer  )
18    constant    F     ( Upper Limit for match-length  )
2     constant    Threshold ( Encode string into position & length  )
                        ( if match-length is greater.  )
N     constant    Nil   ( Index for Binary Search Tree Root  )

variable    textsize    ( text size counter  )
variable    codesize    ( code size counter  )

( These are set by InsertNode procedure.  )

variable    match-position
variable    match-length

: array  create  cells allot     does>   swap cells + ;
: carray create  chars allot     does>   swap chars + ;

N F + 1 -   carray text-buf   ( Ring buffer of size N, with extra  )
                  ( F-1 bytes to facilitate string comparison.  )

( Left & Right Children and Parents -- Binary Search Trees  )

N 1 +   array lson
N 257 +       array rson
N 1 +     array dad

( Input & Output Files  )

0 VALUE     infile  0 VALUE     outfile

\ For i = 0 to N - 1, rson[i] and lson[i] will be the right and
\ left children of node i.  These nodes need not be initialized.
\ Also, dad[i] is the parent of node i.  These are initialized to
\ Nil = N, which stands for `not used.'
\ For i = 0 to 255, rson[N + i + 1] is the root of the tree
\ for strings that begin with character i.  These are initialized
\ to Nil.  Note there are 256 trees. 

( Initialize trees.  )

: inittree            ( --  )
  n 257 +  n 1 +  do    nil  i rson !    loop
  n  0  do    nil  i dad !    loop ;

\ Insert string of length F, text_buf[r..r+F-1], into one of the
\ trees of text_buf[r]'th tree and return the longest-match position
\ and length via the global variables match-position and match-length.
\ If match-length = F, then remove the old node in favor of the new
\ one, because the old one will be deleted sooner.
\ Note r plays double role, as tree node and position in buffer.  )

: insertnode          ( r --  )

  nil over lson !    nil over rson !    0 match-length !
  dup text-buf c@  n +  1 +         ( r p )

  1                 ( r p cmp )
  begin             ( r p cmp )
    0< 0= if           ( r p )

      dup rson @ nil = 0= if
        rson @
      else

        2dup rson !
        swap dad !      (  )
        exit

      then
    else            ( r p )

      dup lson @ nil = 0= if
        lson @
      else

        2dup lson !
        swap dad !      (  )
        exit

      then
    then            ( r p )

    0 f dup 1 do        ( r p 0 f )

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
      f < 0=

    else
      drop false
    then            ( r p cmp flag )
  until             ( r p cmp )
  drop              ( r p )

  2dup dad @ swap dad !
  2dup lson @ swap lson !
  2dup rson @ swap rson !

  2dup lson @ dad !
  2dup rson @ dad !

  dup dad @ rson @ over = if
    tuck dad @ rson !
  else
    tuck dad @ lson !
  then              ( p )

  dad nil swap ! ;    ( remove p  )    (  )

( Deletes node p from tree.  )

: deletenode          ( p --  )

  dup dad @ nil = if    drop exit    then   ( not in tree.  )

  ( case  )              ( p  )
    dup rson @ nil =
  if
    dup lson @
  else
    dup lson @ nil =
  if
    dup rson @
  else

    dup lson @          ( p q  )

    dup rson @ nil = 0= if

      begin
        rson @
        dup rson @ nil =
      until

      dup lson @ over dad @ rson !
      dup dad @ over lson @ dad !

      over lson @ over lson !
      over lson @ dad over swap !
    then

    over rson @ over rson !
    over rson @ dad over swap !

  then then        ( p q  )

  over dad @ over dad !

  over dup dad @ rson @ = if
    over dad @ rson !
  else
    over dad @ lson !
  then              ( p  )

  dad nil swap ! ;           (  )

17 carray   code-buf

variable    len
variable    last-match-length
variable    code-buf-ptr

variable    mask

: encode              ( --  )

  0 textsize !    0 codesize !

  inittree    ( initialize trees.  )

  ( code_buf[1..16] saves eight units of code, and code_buf[0]
  ( works as eight flags, "1" representing that the unit is an
  ( unencoded letter in 1 byte, "0" a position-and-length pair
  ( in 2 bytes.  thus, eight units require at most 16 bytes
  ( of code.  )

  0 0 code-buf c!
  1 mask c!   1 code-buf-ptr !
  0    n f -            ( s r )

  ( clear the buffer with any character that will appear often.  )

  0 text-buf  n f -  bl  fill

  ( read f bytes into the last f bytes of the buffer.  )

  dup text-buf f infile read-file checked   ( s r count )
  dup len !    dup textsize !
  0= if    exit    then         ( s r )

  ( insert the f strings, each of which begins with one or more
  ( `space' characters.  note the order in which these strings
  ( are inserted.  this way, degenerate trees will be less
  ( likely to occur.  )

  f 1 + 1 do            ( s r )
    dup i - insertnode
  loop

  ( finally, insert the whole string just read.  the
  ( global variables match-length and match-position are set.  )

  dup insertnode

  begin             ( s r )

    ( match_length may be spuriously long at end of text.  )
    match-length @ len @ > if    len @ match-length !   then

    match-length @ threshold > 0= if

      ( not long enough match.  send one byte.  )
      1 match-length !
      ( `send one byte' flag  )
      mask c@ 0 code-buf c@ or 0 code-buf c!
      ( send uncoded.  )
      dup text-buf c@ code-buf-ptr @ code-buf c!
      1 code-buf-ptr +!

    else
      ( send position and length pair.
      ( note match-length > threshold.  )

      match-position @  code-buf-ptr @ code-buf c!
      1 code-buf-ptr +!

      match-position @  8 rshift  4 lshift ( . . j )
        match-length @  threshold -  1 -  or
        code-buf-ptr @  code-buf c!  ( . . )
      1 code-buf-ptr +!

    then

    ( shift mask left one bit.  )    ( . . )

    mask c@  2*  mask c!    mask c@ 0= if

      ( send at most 8 units of code together.  )

      0 code-buf  code-buf-ptr @    ( . . a k )
        outfile write-file checked ( . . )
      code-buf-ptr @  codesize  +!
      0 0 code-buf c!    1 code-buf-ptr !    1 mask c!

    then            ( s r )

    match-length @ last-match-length !

    last-match-length @ dup 0 do    ( s r n )

      infile read-char      ( s r n c )
      dup 0< if   2drop i leave   then

      ( delete old strings and read new bytes.  )

      3 pick deletenode
      dup 3 1 + pick text-buf c!

      ( if the position is near end of buffer, extend
      ( the buffer to make string comparison easier.  )

      3 pick f 1 - < if     ( s r n c )
        dup 3 1 + pick n + text-buf c!
      then
      drop          ( s r n )

      ( since this is a ring buffer, increment the
      ( position modulo n.  )

      >r >r         ( s )
        1 +    n 1 - and
      r>            ( s r )
        1 +    n 1 - and
      r>            ( s r n )

      ( register the string in text_buf[r..r+f-1].  )

      over insertnode

    loop            ( s r i )
    dup textsize +!

    ( after the end of text, no need to read, but
    ( buffer may not be empty.  )

    last-match-length @ swap ?do    ( s r )

      over deletenode

      >r  1 +  n 1 - and  r>
      1 +  n 1 - and

      -1 len +!    len @ if
        dup insertnode
      then
    loop

    len @ 0> 0=
  until             2drop

  ( send remaining code.  )

  code-buf-ptr @ 1 > if
    0 code-buf  code-buf-ptr @  outfile  write-file checked
    code-buf-ptr @ codesize +!
  then
;

0 [if]
: statistics          ( --  )
  ." in : " textsize ? cr
  ." out: " codesize ? cr
  textsize @ if
    ." saved: " textsize @ codesize @ - 100 textsize @ */
      2 .r ." %" cr
  then
  infile closed    outfile closed ;
[then]

( Just the reverse of Encode.  )

: decode              ( --  )

  0 text-buf  n f -  bl fill

  0  n f -              ( flags r )
  begin
    >r              ( flags )
      1 rshift dup 256 and 0= if drop     (  )
        infile read-char    ( c  )
        dup 0< if       r> 2drop
          exit      ( c  )
        then
        $FF00 or ( flags  )
        ( uses higher byte to count eight.  )
      then
    r>              ( flags r )

    over 1 and if

      infile read-char      ( . . c  )
      dup 0< if         drop 2drop
        exit        ( . r c  )
      then

      over text-buf c!      ( . r  )
      dup text-buf 1 outfile write-file checked

      1 +    n 1 - and

    else

      infile read-char      ( . . i  )
      dup 0< if         drop 2drop
        exit        ( . r c  )
      then

      infile read-char      ( . . i j  )
      dup 0< if         2drop 2drop
        exit        ( . . i j  )
      then

      dup >r    4 rshift    8 lshift or   r>
      15 and    threshold +    1 +

      0 ?do         ( . r i  )

        dup i +  n 1 - and  text-buf ( . r i a  )
        dup 1 outfile write-file checked
        c@  2 pick text-buf c!  ( . r i  )
        >r  1 +  n 1 - and  r>

      loop          ( . r i  )
      drop          ( flags r  )
    then
  again ;
