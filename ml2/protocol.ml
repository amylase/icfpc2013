open Batteries
open Uint64

type problem = Problem of int * string list

type eval = Eval of int list

type guess = Win | Mismatch of uint64 * uint64 * uint64 | Unknown

let get_problem () =
  let n :: _ :: ops = String.nsplit ~by:" " (read_line ())
  in
  Problem (int_of_string n, ops)

let query lst =
  let n = List.length lst
  in
  print_endline ("eval " ^ string_of_int n ^ " " ^ String.join " " (List.map to_string lst));
  flush stdout;
  let line = read_line () in
  let () = prerr_endline ("evalA: " ^ line) in
  let eval :: _ :: xs = String.nsplit ~by:" " line
  in
  assert(eval = "eval");
  List.map of_string xs

let guess str =
  print_endline ("guess " ^ str);
  flush stdout;
  let str = read_line ()
  in
  let () = prerr_endline ("guessA: " ^ str) in
  if str = "win" then
    Win
  else if str = "unknown" then
    Unknown
  else
    let mismatch :: input :: expected :: your :: _ = String.nsplit ~by:" " str
    in
    assert (mismatch = "mismatch");
    Mismatch (of_string input, of_string expected, of_string your)

