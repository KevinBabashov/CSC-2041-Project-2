(*
This file contains definitions for the OCaml type THING, and the OCaml module
SCANNER, both of which are needed to write the module PARSER for Project 2.
*)
(* THING. Types of the usual Lisp objects. *)
type
thing =
Closure of thing * thing * environment |
Cons of thing * thing |
Nil |
Number of int |
Primitive of (thing -> environment -> thing) |
Symbol of string
and
environment = (string * thing) list ;;
(* SCANNER. Lexical scanner for a subset of Lisp, from the lectures. *)
module Scanner =
struct
(* TOKEN. Expressions are sequences of TOKENs. *)
type token =
CloseParenToken |
EndToken |
NumberToken of int |
OpenParenToken |
SymbolToken of string ;;
(* MAKE SCANNER. Return a version of the scanner function NEXT TOKEN that reads
TOKENs from a file whose pathname is PATH. INPUT is a channel connected to
that file. CH holds the most recently read CHAR from INPUT. *)
let makeScanner path =
let input = open_in path
in let ch = ref ' '
in
(* NEXT CHAR. Advance CH to the next CHAR from INPUT. If there is no next CHAR,
then set CH to '\000'. We use this CHAR to represent the end of a file. *)
let nextChar () =
try ch := input_char input
with End_of_file ->
ch := '\000'
in
(* NEXT CLOSE PAREN TOKEN. Read a CLOSE PAREN TOKEN. *)
let nextCloseParenToken () =
nextChar () ;
CloseParenToken
in
(* NEXT END TOKEN. Read an END TOKEN that indicates the end of INPUT. We don't
skip a CHAR because there are no more CHARs to skip. *)
let nextEndToken () =
EndToken
in
(* NEXT NUMBER TOKEN. Read a NUMBER TOKEN. If it doesn't denote an INT then we
read it as a SYMBOL TOKEN instead. *)
let nextNumberToken () =
let rec nextNumbering chars =
match ! ch
with '\000' | '\n' | ' ' | '(' | ')' ->
(try
NumberToken (int_of_string chars)
with
Failure _ ->
SymbolToken chars) |
_ ->
let otherChars = Char.escaped ! ch
in nextChar () ;
nextNumbering (chars ^ otherChars)
in nextNumbering ""
in
(* NEXT OPEN PAREN TOKEN. Read an OPEN PAREN TOKEN. *)
let nextOpenParenToken () =
nextChar () ;
OpenParenToken
in
(* NEXT SYMBOL TOKEN. Read a SYMBOL TOKEN. *)
let nextSymbolToken () =
let rec nextSymboling chars =
match ! ch
with '\000' | '\n' | ' ' | '(' | ')' ->
SymbolToken chars |
_ ->
let otherChars = Char.escaped ! ch
in nextChar () ;
nextSymboling (chars ^ otherChars)
in nextSymboling ""
in
(* NEXT TOKEN. Look at CH to decide what TOKEN is coming next. Dispatch to the
function that reads the TOKEN and returns it. *)
let rec nextToken () =
match ! ch
with '\000' ->
nextEndToken () |
' ' | '\n' ->
nextChar () ;
nextToken () |
'(' ->
nextOpenParenToken () |
')' ->
nextCloseParenToken () |
'0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '-' ->
nextNumberToken () |
_ ->
nextSymbolToken ()
(* Lost? This is MAKE SCANNER'scanner body. Initialize CH by reading the NEXT CHAR,
and return (but do not call!) the dispatcher NEXT TOKEN. *)
in nextChar () ;
nextToken ;;
end ;;

module type parserish = 
sig 
exception Can'tParse of string
val makeParser : string -> (unit -> thing)
end ;;

module Parser : parserish =
struct 
exception Can'tParse of string

let makeParser path = 
  let scanner = Scanner.makeScanner path in
  let token = ref (scanner ()) in 

  let rec nextThing () = 
    match !token with
    Scanner.CloseParenToken -> raise (Can'tParse "Can't close parenthesis without open parenthesis") |
    Scanner.EndToken -> raise (Can'tParse "Unexpected end of file") | 
    Scanner.NumberToken l -> token := (scanner ()); Number l |
    Scanner.OpenParenToken -> token := (scanner ()); nextThings () |
    Scanner.SymbolToken "nil" -> token := (scanner ()); Nil |
    Scanner.SymbolToken w -> token := (scanner ()); Symbol w 
    
  and nextThings () = 
    match !token with 
    Scanner.CloseParenToken -> token := (scanner ()); Nil |
    Scanner.EndToken -> raise (Can'tParse "Lisp List ended without a Closed Parenthesis") |
    _ -> (* this is a lisp thing we are now evaluating *)
      let first = nextThing () in 
      Cons (first, nextThings ())
  in fun () -> nextThing () ;;
end ;;
