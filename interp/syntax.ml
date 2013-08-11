open Uint64

type id = string
type op1 = Not | Shl1 | Shr1 | Shr4 | Shr16
type op2 = And | Or | Xor | Plus
type expr =
| Zero
| One
| Var of id
| If0 of expr * expr * expr
| Fold of expr * expr * expr
| Op1 of op1 * expr
| Op2 of op2 * expr * expr
type program = Lambda of expr

let op1_to_string = function
  | Not -> "not"
  | Shl1 -> "shl1"
  | Shr1 -> "shr1"
  | Shr4 -> "shr4"
  | Shr16 -> "shr16"

let op2_to_string = function
  | And -> "and"
  | Or  -> "or"
  | Xor -> "xor"
  | Plus -> "plus"

let rec expr_to_string = function
  | Zero   -> "0"
  | One    -> "1"
  | Var id -> id
  | If0 (e1, e2, e3) -> "(if0 " ^ expr_to_string e1 ^ " " ^ expr_to_string e2 ^ " " ^ expr_to_string e3 ^ ")"
  | Fold (e1, e2, e3) -> "(fold " ^ expr_to_string e1 ^ " " ^ expr_to_string e2 ^ " (lambda (y z) " ^ expr_to_string e3 ^ "))"
  | Op1 (op, e) -> "(" ^ op1_to_string op ^ " " ^ expr_to_string e ^ ")"
  | Op2 (op, e1, e2) -> "(" ^ op2_to_string op ^ " " ^ expr_to_string e1 ^ " " ^ expr_to_string e2 ^ ")"

let program_to_string (Lambda e) =
  "(lambda (x) " ^ expr_to_string e ^ ")"

let rec uint_fold' f init vec n =
  if n = 0 then
    init
  else
    uint_fold' f (f (logand vec (of_int 0xff)) init) (shift_right vec 8) (n-1)

let uint_fold f init vec =
  uint_fold' f init vec 8

let rec eval_expr x y z = function
  | Zero -> zero
  | One  -> one
  | Var id ->
    begin match id with
    | "x" -> x
    | "y" -> y
    | "z" -> z
    | _ -> assert false
    end
  | If0 (e1, e2, e3) ->
    if eval_expr x y z e1 = zero then
      eval_expr x y z e2
    else
      eval_expr x y z e3
  | Fold (e1, e2, e3) ->
    uint_fold (fun y z -> eval_expr x y z e3) (eval_expr x y z e2) (eval_expr x y z e1)
  | Op1 (op, e1) ->
    let v = (eval_expr x y z e1) in
    begin match op with
    | Not -> lognot v
    | Shl1 -> shift_left v 1
    | Shr1 -> shift_right v 1
    | Shr4 -> shift_right v 4
    | Shr16 -> shift_right v 16
    end
  | Op2 (op, e1, e2) ->
    let v1 = eval_expr x y z e1 in
    let v2 = eval_expr x y z e2 in
    begin match op with
    | And -> logand v1 v2
    | Or  -> logor  v1 v2
    | Xor -> logxor v1 v2
    | Plus -> add v1 v2
    end

let eval_program x (Lambda e) = eval_expr x zero zero e
