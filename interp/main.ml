open Uint64
open Syntax
open Lexing

open Batteries

let () =
  let _ :: input :: program = Array.to_list Sys.argv
  in
  let str = String.join " " program
  in
  print_endline (to_string (eval_program (of_string input) (Parser.program Lexer.token (Lexing.from_string str))))
