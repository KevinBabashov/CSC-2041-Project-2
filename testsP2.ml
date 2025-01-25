(*
   TESTS. Tests for Project 2.

     James Moen
     08 Nov 24

   Unlike the tests for a lab, these are not worth points! Instead, they help
   test if your parser works correctly.
*)

(* Make a parser NEXT THING that reads from the file "things.txt". *)

let nextThing = Parser.makeParser "things.txt" ;;

(* Each call to NEXT THING reads a Lisp expression, constructs an equivalent
   OCaml object, and returns that object. The comment following each call shows
   what OCaml will print if NEXT THING works correctly. These are the same
   Lisp expressions that were used to test your print function from Lab 9. *)

nextThing () ;;  (* nil *)

(* - : thing = Nil *)

nextThing () ;;  (* 7734 *)

(* - : thing = Number 7734 *)

nextThing () ;;  (* lobyms *)

(* - : thing = Symbol "lobyms" *)

nextThing () ;;  (* (a) *)

(* - : thing = Cons (Symbol "a", Nil) *)

nextThing () ;;  (* (a b) *)

(* - : thing = Cons (Symbol "a", Cons (Symbol "b", Nil)) *)

nextThing () ;;  (* (a b c) *)

(* - : thing = Cons (Symbol "a", Cons (Symbol "b", Cons (Symbol "c", Nil))) *)

nextThing () ;;  (* ((a) b c) *)

(* - : thing =
   Cons (Cons (Symbol "a", Nil), Cons (Symbol "b", Cons (Symbol "c", Nil))) *)

nextThing () ;;  (* ((a b) c) *)

(* - : thing =
   Cons (Cons (Symbol "a", Cons (Symbol "b", Nil)), Cons (Symbol "c", Nil)) *)

nextThing () ;;  (* a (b c) *)

(* - : thing =
   Cons (Symbol "a", Cons (Cons (Symbol "b", Cons (Symbol "c", Nil)), Nil)) *)

nextThing () ;;  (* ((a b c)) *)

(* - : thing =
   Cons (Cons (Symbol "a", Cons (Symbol "b", Cons (Symbol "c", Nil))), Nil) *)

nextThing () ;;  (* (define ! (lambda (n) (if (= n 0) 1 (∗ n (! (− n 1)))))) *)

(* - : thing =
   Cons (Symbol "define",
    Cons (Symbol "!",
     Cons
      (Cons (Symbol "lambda",
        Cons (Cons (Symbol "n", Nil),
         Cons
          (Cons (Symbol "if",
            Cons (Cons (Symbol "=", Cons (Symbol "n", Cons (Number 0, Nil))),
             Cons (Number 1,
              Cons
               (Cons (Symbol "*",
                 Cons (Symbol "n",
                  Cons
                   (Cons (Symbol "!",
                     Cons
                      (Cons (Symbol "-",
                        Cons (Symbol "n", Cons (Number 1, Nil))),
                      Nil)),
                   Nil))),
               Nil)))),
          Nil))),
      Nil))) *)

(* At this point, we've read all the Lisp expressions from "things.txt", so if
   you call NEXT THING again, it should raise the exception CAN'T PARSE. This
   is what your code is supposed to do! *)

nextThing () ;;

(* Exception: Parser.Can'tParse "Unexpected end of file". *)
