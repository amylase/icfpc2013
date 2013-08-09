open MyBatteries
open MyStd
open Protocol
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

let rec uint_fold' f init vec n =
  if n = 0 then
    init
  else
    uint_fold' f (f (logand vec (of_int 0xff)) init) (shift_right vec 1) (n-1)

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
  | Fold (e1, e2, e3) -> "fold " ^ expr_to_string e1 ^ " " ^ expr_to_string e2 ^ " (lambda (y, z) " ^ expr_to_string e3 ^ ")"
  | Op1 (op, e) -> "(" ^ op1_to_string op ^ " " ^ expr_to_string e ^ ")"
  | Op2 (op, e1, e2) -> "(" ^ op2_to_string op ^ " " ^ expr_to_string e1 ^ " " ^ expr_to_string e2 ^ ")"

let program_to_string (Lambda e) =
  "(lambda (x) " ^ expr_to_string e ^ ")"

let split2 n =
  if n < 2 then
    Enum.empty ()
  else
    List.enum (List.map (fun x -> (x, n - x)) (List.init (n-1) ((+)1)))

let split3 n =
  if n < 3 then
    Enum.empty ()
  else
    [? (x, y, z) | x <- Enum.init (n-2) ((+)1);
                   (y, z) <- split2 (n-x)
    ?]

let rec can_optimize_expr = function
  | Zero -> false
  | One -> false
  | Var id -> false
  | If0 (e1, e2, e3) ->
    can_optimize_expr e1 || can_optimize_expr e2 || can_optimize_expr e3 || e2 = e3
  | Fold (e1, e2, e3) ->
    can_optimize_expr e1 || can_optimize_expr e2 || can_optimize_expr e3
  | Op1 (op, e) ->
    can_optimize_expr e ||
      begin match op, e with
      | Not, Op1 (Not, _) -> true
      | _ -> false
      end
  | Op2 (op, e1, e2) ->
    can_optimize_expr e1 || can_optimize_expr e2 || e1 > e2 ||
      begin match op, e1, e2 with
      | And, Zero, _
      | And, _, Zero
      | And, One, One
      | Or, Zero, _
      | Or, _, Zero
      | Or, One, One
      | Xor, Zero, _
      | Xor, _, Zero
      | Xor, One, One
      | Plus, Zero, _
      | Plus, _, Zero -> true
      | _ -> false
      end

let memo = Hashtbl.create 0

let rec enumerate_expr fold bound n =
  if Hashtbl.mem memo (fold, bound, n) then
    Hashtbl.find memo (fold, bound, n)
  else
    let ans = Set.filter (not % can_optimize_expr) begin match n with
      | 0 -> invalid_arg "zero is not allowed"
      | 1 ->
        Set.of_list
          (if bound then [Zero; One; Var "x"; Var "y"; Var "z"] else [Zero; One; Var "x"])
      | n ->
        if fold then
          Set.unions [
            [? Set : Op1 (op, e) |
                op <- List.enum [Not; Shl1; Shr1; Shr4; Shr16];
                e <- Set.enum (enumerate_expr fold bound (n-1))
            ?];
            [? Set : Op2 (op, e1, e2) |
                op <- List.enum [And; Or; Xor; Plus];
                (x, y) <- split2 (n-1);
                e1 <- Set.enum (enumerate_expr fold bound x);
                e2 <- Set.enum (enumerate_expr false bound y)
            ?];
            [? Set : Op2 (op, e1, e2) |
                op <- List.enum [And; Or; Xor; Plus];
                (x, y) <- split2 (n-1);
                e1 <- Set.enum (enumerate_expr false bound x);
                e2 <- Set.enum (enumerate_expr fold bound y)
            ?];
            [? Set : If0 (e1, e2, e3) |
                (x, y, z) <- split3 (n-1);
                e1 <- Set.enum (enumerate_expr fold bound x);
                e2 <- Set.enum (enumerate_expr false bound y);
                e3 <- Set.enum (enumerate_expr false bound z)
            ?];
            [? Set : If0 (e1, e2, e3) |
                (x, y, z) <- split3 (n-1);
                e1 <- Set.enum (enumerate_expr false bound x);
                e2 <- Set.enum (enumerate_expr fold bound y);
                e3 <- Set.enum (enumerate_expr false bound z)
            ?];
            [? Set : If0 (e1, e2, e3) |
                (x, y, z) <- split3 (n-1);
                e1 <- Set.enum (enumerate_expr false bound x);
                e2 <- Set.enum (enumerate_expr false bound y);
                e3 <- Set.enum (enumerate_expr fold bound z)
            ?];
            [? Set : Fold (e1, e2, e3) |
               (x, y, z) <- split3 (n-2);
               e1 <- Set.enum (enumerate_expr false false x);
               e2 <- Set.enum (enumerate_expr false false y);
               e3 <- Set.enum (enumerate_expr false true z)
            ?]
          ]
        else
          Set.unions [
            [? Set : Op1 (op, e) |
                op <- List.enum [Not; Shl1; Shr1; Shr4; Shr16];
                e <- Set.enum (enumerate_expr fold bound (n-1))
            ?];
            [? Set : Op2 (op, e1, e2) |
                op <- List.enum [And; Or; Xor; Plus];
                (x, y) <- split2 (n-1);
                e1 <- Set.enum (enumerate_expr fold bound x);
                e2 <- Set.enum (enumerate_expr fold bound y)
            ?];
            [? Set : If0 (e1, e2, e3) |
                (x, y, z) <- split3 (n-1);
                e1 <- Set.enum (enumerate_expr fold bound x);
                e2 <- Set.enum (enumerate_expr fold bound y);
                e3 <- Set.enum (enumerate_expr fold bound z)
            ?]
          ]
    end
    in
    Hashtbl.add memo (fold, bound, n) ans; ans

let enumerate_tfold_program n =
  [? Set : Lambda (Fold (e1, e2, e3)) |
     (x, y, z) <- split3 (n-2);
     e1 <- Set.enum (enumerate_expr false false x);
     e2 <- Set.enum (enumerate_expr false false y);
     e3 <- Set.enum (enumerate_expr false true z)
  ?]

let enumerate_program n =
  Set.map (fun e -> Lambda e) (enumerate_expr true false (n-1))

let rec f i candidates =
  if Set.cardinal candidates = 1 then
    Set.choose candidates
  else
    let evalQ = (List.init 256 (fun j -> of_int (256 * i + j))) in
    let evalA = eval evalQ in
    f (i+1) (List.fold_left (fun set (q, a) -> Set.filter (fun p -> eval_program q p = a) set) candidates (List.combine evalQ evalA))

let () =
  let Problem (n, ops) = get_problem () in
  let enumerate = if Array.mem "-tfold" Sys.argv then enumerate_tfold_program else enumerate_program
  in
  let candidates = [? Set : p | i <- (2--n); p <- Set.enum (enumerate i) ?]
  in
  if Array.mem "-v" Sys.argv then
    Set.iter (print_endline % program_to_string) candidates;
  prerr_endline (string_of_int (Set.cardinal candidates));
  assert (guess (program_to_string (f 0 candidates)) = Win)

(*
let () =
  print_endline (to_string_hex (eval_expr (of_int 0x1122334455667788) zero zero (Fold ((Var "x"), Zero, (Op2 (Or, (Var "y"), (Var "z")))))));
  flush stdout;
  Scanf.scanf "%d" solve*)
