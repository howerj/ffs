\ ==================== Matcher ===============================
\ ( The following section implements a very simple regular
\ expression engine, which expects an ASCIIZ Forth string. It
\ is translated from C code and performs an identical function.
\ 
\ The regular expression language is as follows:
\ 
\ 	c	match a literal character
\ 	.	match any character
\ 	*	match any characters
\ 
\ The "*" operator performs the same function as ".*" does in
\ most other regular expression engines. Most other regular
\ expression engines also do not anchor their selections to the
\ beginning and the end of the string to match, instead using
\ the operators '^' and '$' to do so, to emulate this behavior
\ '*' can be added as either a suffix, or a prefix, or both,
\ to the matching expression.
\ 
\ As an example "*, World!" matches both "Hello, World!" and
\ "Good bye, cruel World!". "Hello, ...." matches "Hello, Bill"
\ and "Hello, Fred" but not "Hello, Tim" as there are two few
\ characters in the last string. )
\ 
\ Translated from http://c-faq.com/lib/regex.html
\ int match(char *pat, char *str)
\ {
\   switch(*pat) {
\   case '\0':  return !*str;
\   case '*':   return match(pat+1, str) || *str && match(pat, str+1);
\   case '.':   return *str && match(pat+1, str+1);
\   default:    return *pat == *str && match(pat+1, str+1);
\   }
\ }

: *pat ( regex -- regex char : grab next character of pattern )
  dup c@ ;

: *str ( string regex -- string regex char : grab next character string to match )
  over c@ ;

: pass ( c-addr1 c-addr2 -- bool : pass condition, characters matched )
  2drop 1 ;

: reject ( c-addr1 c-addr2 -- bool : fail condition, character not matched )
  2drop 0 ;

: *pat==*str ( c-addr1 c-addr2 -- c-addr1 c-addr2 bool )
  2dup c@ swap c@ = ;

: ++ ( u1 u2 u3 u4 -- u1+u3 u2+u4 : not quite d+ [does no carry] )
  swap >r + swap r> + swap ;

defer matcher

: advance ( string regex char -- bool : advance both regex and string )
  if 1 1 ++ matcher else reject then ;

: advance-string ( string regex char -- bool : advance only the string )
  if 1 0 ++ matcher else reject then ;

: advance-regex ( string regex -- bool : advance matching )
  2dup 0 1 ++ matcher if pass else *str advance-string then ;

: match ( string regex -- bool : match a ASCIIZ pattern against an ASCIIZ string )
  *pat
  case
           0 of drop c@ not   endof
    [char] * of advance-regex endof
    [char] . of *str advance  endof
    
    drop *pat==*str advance exit

  endcase ;

matcher is match

\ CCITT

( Make a word to limit arithmetic to a 16-bit value )
size 2 = [if]
	: limit immediate ;  ( do nothing, no need to limit )
[else]
	: limit 0xffff and ; ( limit to 16-bit value )
[then]

: ccitt ( crc c-addr -- crc : calculate polynomial 0x1021 AKA "x16 + x12 + x5 + 1" )
	c@                         ( get char )
	limit over 256/ xor        ( crc x )
	dup  4  rshift xor         ( crc x )
	dup  5  lshift limit xor   ( crc x )
	dup  12 lshift limit xor   ( crc x )
	swap 8  lshift limit xor ; ( crc )

( See http://stackoverflow.com/questions/10564491
  and https://www.lammertbies.nl/comm/info/crc-calculation.html )
: crc16-ccitt ( c-addr u -- u )
	0xffff -rot
	['] ccitt foreach ;
hide{ limit ccitt }hide

( ==================== Date ================================== )
( This word set implements a words for date processing, so
you can print out nicely formatted date strings. It implements
the standard Forth word time&date and two words which interact
with the libforth DATE instruction, which pushes the current
time information onto the stack. )


: >month ( month -- c-addr u : convert month to month string )
	case
		 1 of c" Jan " endof
		 2 of c" Feb " endof
		 3 of c" Mar " endof
		 4 of c" Apr " endof
		 5 of c" May " endof
		 6 of c" Jun " endof
		 7 of c" Jul " endof
		 8 of c" Aug " endof
		 9 of c" Sep " endof
		10 of c" Oct " endof
		11 of c" Nov " endof
		12 of c" Dec " endof
		-11 throw
	endcase ;

: .day ( day -- c-addr u : add ordinal to day )
	10 mod
	case
		1 of c" st " endof
		2 of c" nd " endof
		3 of c" rd " endof
		drop c" th " exit
	endcase ;

: >day ( day -- c-addr u: add ordinal to day of month )
	dup  1 10 within if .day   exit then
	dup 10 20 within if drop c" th " exit then
	.day ;

: >weekday ( weekday -- c-addr u : print the weekday )
	case
		0 of c" Sun " endof
		1 of c" Mon " endof
		2 of c" Tue " endof
		3 of c" Wed " endof
		4 of c" Thu " endof
		5 of c" Fri " endof
		6 of c" Sat " endof
		-11 throw
	endcase ;

: >gmt ( bool -- GMT or DST? )
	if c" DST " else c" GMT " then ;

: colon ( -- char : push a colon character )
	[char] : ;

: 0? ( n -- : hold a space if number is less than base )
	(base) u< if [char] 0 hold then ;

( .NB You can format the date in hex if you want! )
: date-string ( date -- c-addr u : format a date string in transient memory )
	9 reverse ( reverse the date string )
	<#
		dup #s drop 0? ( seconds )
		colon hold
		dup #s drop 0? ( minute )
		colon hold
		dup #s drop 0? ( hour )
		dup >day holds
		#s drop ( day )
		>month holds
		bl hold
		#s drop ( year )
		>weekday holds
		drop ( no need for days of year )
		>gmt holds
		0
	#> ;

: .date ( date -- : print the date )
	date-string type ;

: time&date ( -- second minute hour day month year )
	date
	3drop ;

hide{ >weekday .day >day >month colon >gmt 0? }hide

( ==================== Date ================================== )

