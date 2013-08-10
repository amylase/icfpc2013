type id = string
type size = int
type fold = bool
type bound = bool
type tree_characteristics = size * fold * bound
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
| Tree of tree_characteristics
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

let string_to_op1 = function
  | "not"   -> Not
  | "shl1"  -> Shl1
  | "shr1"  -> Shr1
  | "shr4"  -> Shr4
  | "shr16" -> Shr16
  | x -> invalid_arg (x ^ " is not op1.")

let string_to_op2 = function
  | "and"  -> And
  | "or"   -> Or
  | "xor"  -> Xor
  | "plus" -> Plus
  | x -> invalid_arg (x ^ " is not op2.")

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

let rec complete = function
  | Tree _ -> false
  | Zero | One | Var _ -> true
  | Op1 (_, e) -> complete e
  | Op2 (_, e1, e2) -> complete e1 && complete e2
  | If0 (e1, e2, e3) | Fold (e1, e2, e3) -> complete e1 && complete e2 && complete e3
