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

