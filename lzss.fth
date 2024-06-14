\ LZSS -- A Data Compression CODEC
\ 89-04-06 Standard C by Haruhiko Okumura
\ 94-12-09 Standard Forth by Wil Baden
\ 21-02-20 Personalized Forth by Richard Howe
\ Use, distribute, and modify this program freely.
\ TODO:
\ - Refactor so this is more "forth" like, this looks like it is translated
\   from C.
\ - Implement entirely memory only version
\ - Separate encode from decode so they can be used independently

( only forth also definitions ) decimal

: array  create cells allot does> swap cells + ;
: carray create chars allot does> swap chars + ;
: checked abort" file access error. " ;
: closed close-file throw ;
: slurp ( b-mem u-mem b-name u-name -- u )
  r/o open-file throw dup >r read-file checked r> close-file throw ;
: save ( b-mem u-mem b-name u-name -- )
  w/o open-file throw dup >r write-file checked r> close-file throw ;

32678 constant mem-sz
variable in-mem mem-sz chars allot
variable in-mem-used
variable in-mem-index
variable out-mem mem-sz chars allot
variable out-mem-used
variable out-mem-index

: get ( -- char|-1 )
   in-mem-index @ in-mem-used @ >= if -1 exit then
   in-mem-index @ chars in-mem + c@ 1 in-mem-index +! ;
: put
  out-mem-index @ out-mem-used @ >= if drop -1 exit then
  out-mem-used @ chars out-mem + c! 1 out-mem-index +! 0 ;
: gets ( b u -- u )
  0 >r
  begin
   ?dup
  while
   get dup 0< if drop 2drop r> exit then
   over + c!
   r> 1+ >r
   +string
  repeat drop r> ;
: puts begin ?dup while over + c@ put checked +string repeat drop ; ( b u -- )

variable in-file variable out-file
create single-char-i/o-buffer 0 c, align
: read-char ( file -- char|-1 )
  single-char-i/o-buffer 1 rot read-file checked if
    single-char-i/o-buffer c@
  else -1 then ;
: get in-file @ read-char ;
: gets  in-file @ read-file checked ;
: put 1 out-file @ write-file checked ;
: puts out-file @ write-file checked ;

4096 constant n         \ size of ring buffer; must be power of 2
18   constant f         \ upper limit for match-length
2    constant threshold \ encode string into position & length
                        \ if match-length is greater.
n    constant null      \ index for binary search tree root

: nwrap [ n 1- ] literal and ;

variable text-size \ text size counter
variable code-size \ code size counter

\ These are set by insert-node procedure.

variable match-position
variable match-length

create window n f + 1- chars allot
: text-buf window swap chars + ;

\ Left and Right Children and Parents -- Binary Search Trees

n 1+    array lson
n 257 + array rson
n 1+    array dad

\ For i = 0 to N - 1, rson[i] and lson[i] will be the right and left children
\ of node i.  These nodes need not be initialized.  Also, dad[i] is the
\ parent of node i.  These are initialized to null = N, which stands for
\ 'not used.'  For i = 0 to 255, rson[N + i + 1] is the root of the tree
\ for strings that begin with character i.  These are initialized to "null".
\ Note there are 256 trees.

: init-tree n 257 + n 1+ do null i rson ! loop n for null r@ dad ! next ;

\ Insert string of length F, text_buf[r..r+F-1], into one of the trees of
\ text_buf[r]'th tree and return the longest-match position and length via
\ the global variables match-position and match-length.  If match-length = F,
\ then remove the old node in favor of the new one, because the old one will
\ be deleted sooner.  Note r plays double role, as tree node and position
\ in buffer.

: insert-node                              ( r -- )
  null over lson ! null over rson ! 0 match-length !
  dup text-buf c@ n + 1+                   ( r p )
  1                                        ( r p cmp )
  begin                                    ( r p cmp )
    0>= if                               ( r p )
      dup rson @ null = 0= if
      rson @
      else
        2dup rson !
        swap dad !                         ( )
        exit
      then
    else                                   ( r p )
      dup lson @ null = 0= if
        lson @
      else
        2dup lson !
        swap dad !                         ( )
        exit
      then
    then                                   ( r p )
    0 f dup 1 do                           ( r p 0 f )
      3 pick i + text-buf c@               ( r p 0 f c )
      3 pick i + text-buf c@ -             ( r p 0 f diff )
      ?dup if nip nip i leave then         ( r p 0 f )
    loop                                   ( r p cmp i )
    dup match-length @ > if
      2 pick match-position !
      dup match-length !
      f < 0=
      else
        drop 0
    then                                   ( r p cmp flag )
  until                                    ( r p cmp )
  drop                                     ( r p )

  2dup dad @ swap dad !
  2dup lson @ swap lson !
  2dup rson @ swap rson !

  2dup lson @ dad !
  2dup rson @ dad !

  dup dad @ rson @ over = if
    tuck dad @ rson !
  else
    tuck dad @ lson !
  then                                    ( p )
  dad null swap ! ;   ( remove p )        ( )

: delete-node ( p -- : deletes node p from tree )
  dup dad @ null = if drop exit then   ( not in tree )
  ( case )                             ( p )
  dup rson @ null =
  if dup lson @
  else
    dup lson @ null =
    if dup rson @
    else
      dup lson @                       ( p q )
      dup rson @ null = 0= if
      begin
        rson @
        dup rson @ null =
      until
      dup lson @ over dad @ rson !
      dup dad @ over lson @ dad !
      over lson @ over lson !
      over lson @ dad over swap !
    then
    over rson @ over rson !
    over rson @ dad over swap !
  ( esac ) then then                   ( p q )
  over dad @ over dad !
  over dup dad @ rson @ = if
    over dad @ rson !
  else
    over dad @ lson !
  then                                 ( p )
  dad null swap ! ;                    ( )

17 carray code-buf
variable len
variable last-match-length
variable code-buf-ptr
variable mask

: lzss-encode ( -- )
  0 text-size ! 0 code-size !
  init-tree
  \ code-buf[1..16] saves eight units of code, and code-buf[0] works as eight
  \ flags, "1" representing that the unit is an unencoded letter in 1 byte,
  \ "0" a position-and-length pair in 2 bytes. Thus, eight units require at
  \ most 16 bytes of code.
  0 0 code-buf c!
  1 mask c! 1 code-buf-ptr !
  0 n f -                ( s r )
  \ clear the buffer with any character that will appear often.
  0 text-buf n f - bl fill
  \ read f bytes into the last f bytes of the buffer.
  dup text-buf f gets    ( s r count )
  dup len ! dup text-size !
  0= if exit then                    ( s r )
  \ insert the f strings, each of which begins with one or more 'space'
  \ characters. Note the order in which these strings are inserted. This way,
  \ degenerate trees will be less likely to occur.
  f 1+ 1 do dup i - insert-node loop ( s r )
  \ finally, insert the whole string just read. The
  \ global variables match-length and match-position are set.
  dup insert-node
    begin                                     ( s r )
      \ match_length may be spuriously long at end of text.
      match-length @ len @ > if len @ match-length ! then
      match-length @ threshold <= if
        1 match-length ! \ Not long enough match. Send one byte.
        mask c@ 0 code-buf c@ or 0 code-buf c! \ 'send one byte' flag
        dup text-buf c@ code-buf-ptr @ code-buf c! \ send unencoded.
        1 code-buf-ptr +!
      else
        \ send position and length pair. Note match-length > threshold.
        match-position @ code-buf-ptr @ code-buf c!
        1 code-buf-ptr +!
        match-position @ 8 rshift 4 lshift ( . . j )
        match-length @ threshold -  1-  or
        code-buf-ptr @ code-buf c!  ( . . )
        1 code-buf-ptr +!
      then
      \ shift mask left one bit.          ( . . )
      mask c@  2* mask c! mask c@ 0= if
        \ send at most 8 units of code together.
        0 code-buf  code-buf-ptr @        ( . . a k )
        puts                              ( . . )
        code-buf-ptr @  code-size +!
        0 0 code-buf c! 1 code-buf-ptr ! 1 mask c!
      then                                ( s r )
      match-length @ last-match-length !
      last-match-length @ dup 0 do        ( s r n )
        get                               ( s r n c )
        dup 0< if 2drop i leave then
        \ delete old strings and read new bytes.
        3 pick delete-node
        dup 3 1+ pick text-buf c!
        \ if position is near end of buffer, extend it so comparison is easier
        3 pick f 1- < if dup 3 1+ pick n + text-buf c! then ( s r n c )
        drop                          ( s r n )
        \ since this is a ring buffer, increment the position modulo n.
        >r >r                         ( s )
        1+ nwrap
        r>                            ( s r )
        1+ nwrap
        r>                            ( s r n )
        \ register the string in text_buf[r..r+f-1].
        over insert-node
      loop                                ( s r i )
      dup text-size +!
      \ after the end of text, no need to read, but buffer may not be empty.
      last-match-length @ swap ?do        ( s r )
        over delete-node
        >r 1+ nwrap r>
        1+ nwrap
        -1 len +! len @ if dup insert-node then
      loop
      len @ 0<=
    until 2drop
  \ send remaining code.
  code-buf-ptr @ 1 > if
    0 code-buf code-buf-ptr @ puts
    code-buf-ptr @ code-size +!
  then ;

: stats ( -- )
  ." In : " text-size ? cr
  ." Out: " code-size ? cr
  text-size @ if
    ." Saved: " text-size @ code-size @ - 100 text-size @ */
    2 .r ." %" cr
  then ;

\ Just the reverse of Encode.

: lsb 1 and ;
: lzss-decode ( -- )
  0 text-buf n f -  bl fill
  0 n f -                             ( flags r )
  begin
    >r                                ( flags )
    1 rshift dup 256 and 0= if drop   ( )
      get                             ( c )
      dup 0< if r> 2drop exit then    ( c )
      $FF00 or                        ( flags )
      \ uses higher byte to count eight
    then
    r>                                ( flags r )
    over lsb if
      get                             ( . . c )
      dup 0< if drop 2drop exit then  ( . r c )
      over text-buf c!                ( . r )
      dup text-buf put
      1+ nwrap
    else
      get                             ( . . i )
      dup 0< if drop 2drop exit then  ( . r c )
      get                             ( . . i j )
      dup 0< if 2drop 2drop exit then ( . . i j )
      dup >r 4 rshift 8 lshift or r>
      15 and threshold + 1+
      0 ?do                           ( . r i )
        dup i + nwrap text-buf        ( . r i a )
        dup put
        c@ 2 pick text-buf c!         ( . r i )
        >r 1+ nwrap r>
      loop                            ( . r i )
      drop                            ( flags r )
    then
  again ;

in-mem mem-sz s" help.txt" slurp ." read> " . cr
s" help.txt"      r/o open-file throw in-file !
s" help.txt.lzss" w/o create-file throw out-file !
lzss-encode
in-file @ closed out-file @ closed
s" help.txt.lzss" r/o open-file throw in-file !
s" help.txt.orig" w/o create-file throw out-file !
lzss-decode
in-file @ closed out-file @ closed
stats
