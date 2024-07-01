\ Author: Richard James Howe
\ License: The Unlicense / Public Domain
\ Email: howe.r.j.89@gmail.com
\ Repo: https://github.com/howerj/lzp-forth

\ TODO: Get working, port to SUBLEQ eForth, vocabularies,
\ port to FFS, find suitable buffer for SUBLEQ eForth, try
\ different models for SUBLEQ eForth

\ wordlist constant {lzp} {lzp} +order definitions

defined recreate-file 0= [if] 
 : recreate-file create-file ; 
[then]

defined eforth [if] 8 [else] 16 [then] constant model-bits
1 model-bits lshift constant model-size
create model model-size allot align
create buf 9 allot align
variable bufp
variable end
variable infile
variable outfile
variable ocnt
variable icnt
variable prediction
variable mask
variable run

defined +hash 0= [if] ( default hash model )
: +hash  ( h c -- h )
  swap 4 lshift xor [ model-size 1- ] literal and ; 
[then]

\ : get ( file -- c|-1 )
\  infile @ file-eof? if -1 end ! -1 exit then
\  infile @ key-file dup 0< if end ! exit then 1 icnt +! ;
: get ( file -- c|-1 )
  infile @ key-file dup 0< if end ! exit then 
  infile @ file-eof? if drop -1 end ! -1 exit then
  1 icnt +! ;

: put outfile @ emit-file throw 1 ocnt +! ; ( c -- )
: predict prediction @ swap +hash prediction ! ; ( c -- )
: reset
  0 prediction !
  0 end ! 
\  -1 infile ! -1 outfile !
  0 icnt ! 0 ocnt !
  buf 9 erase
  model model-size erase ;
: model? prediction @ model + c@ ; ( -- c )
: model! prediction @ model + c! ; ( c -- )
: breset 1 bufp ! 0 buf c! 0 run ! ;
: wbuf 
  run @ if
    buf bufp @ 1- for
      count put
    next drop
  then breset ;
: lzp-encode ( -- ior )
  reset
  begin
    breset
    0 begin
      dup 8 < end @ 0= and
    while
      get >r r@ 0< if wbuf rdrop drop 0 exit then
      -1 run !
      r@ model? = if
        dup 1 swap lshift buf c@ or buf c!
      else
        r@ model! 
        r@ buf bufp @ + c! 
        1 bufp +!
      then 
      r> predict
      1+
    repeat drop
    wbuf
  end @ until wbuf 0 ;
: lzp-decode ( -- ior )
  reset
  begin
    get dup 0< if drop 0 exit then
    0 begin
      dup 8 <
    while
      2dup 1 swap lshift and if
        model?
      else
        get dup 0< if drop 2drop 0 exit then
        dup model!
      then
      dup put predict
      1+
    repeat 2drop
  end @ until 0 ;

: lzp-statistics ( -- )
  ." in : " icnt ? cr
  ." out: " ocnt ? cr
  icnt @ if
    ." saved: " icnt @ ocnt @ - 100 icnt @ */
      2 .r ." %" cr
  then ;

: lzp-file outfile ! infile ! lzp-encode ;
: unlzp-file outfile ! infile ! lzp-decode ;
  
: lzp-file-name ( "from" "to" -- ior )
  w/o recreate-file ?dup if nip exit then outfile !
  r/o open-file ?dup if nip outfile @ close-file drop exit then
  infile !
  [ ' lzp-encode ] literal catch
  infile @ close-file 0 infile !
  outfile @ close-file 0 outfile !
  ?dup if nip nip exit then
  ?dup if nip exit then ;

: unlzp-file-name ( "from" "to" -- ior )
  w/o recreate-file ?dup if nip exit then outfile !
  r/o open-file ?dup if nip infile @ close-file drop exit then
  infile !
  [ ' lzp-decode ] literal catch
  infile @ close-file 0 infile !
  outfile @ close-file 0 outfile !
  ?dup if nip nip exit then
  ?dup if nip exit then ;

defined +ffs [if]
\ Forth File System <https://github.com/howerj/ffs> extensions.
+ffs
: lzp ( "from" "to" -- )
  locked!? ro? 
  narg mcopy narg movebuf namebuf lzp-file-name ;
: unlzp ( "from" "to" -- ) 
  locked!? ro? 
  narg mcopy narg movebuf namebuf unlzp-file-name ;

s" help.txt"  s" help.lzp" lzp-file-name throw lzp-statistics
s" help.lzp" s" help.orig" unlzp-file-name throw lzp-statistics
[else]
\ If `+ffs` is not defined we are most likely running gforth
\ with the File Access Methods that talk to the normal systems
\ file system, the file `lzp.fth` and `ffs.fth` should be
\ present instead.

s" lzp.fth"  s" lzp.lzp" lzp-file-name throw lzp-statistics
s" lzp.lzp" s" lzp.orig" unlzp-file-name throw lzp-statistics

s" ffs.fb"  s" ffs.lzp" lzp-file-name throw lzp-statistics
s" ffs.lzp" s" ffs.orig" unlzp-file-name throw lzp-statistics


[then]

bye
