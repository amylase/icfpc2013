open MyBatteries
open Uint64

type problem = Problem of int * string list

type eval = Eval of int list

type guess = Win | Mismatch of int * int * int

let get_problem () =
  let n :: _ :: ops = String.nsplit ~by:" " (read_line ())
  in
  Problem (int_of_string n, ops)

let eval lst =
  let n = List.length lst
  in
  print_endline ("eval " ^ string_of_int n ^ " " ^ String.join " " (List.map to_string lst));
  flush stdout;
  let eval :: _ :: xs = String.nsplit ~by:" " (read_line ())
  in
  assert(eval = "eval");
  List.map of_string xs

let guess str =
  print_endline ("guess " ^ str);
  flush stdout;
  let str = read_line ()
  in
  if str = "win" then
    Win
  else
    let mismatch :: input :: expected :: your :: _ = String.nsplit ~by:" " str
    in
    assert (mismatch = "mismatch");
    Mismatch (int_of_string input, int_of_string expected, int_of_string your)

