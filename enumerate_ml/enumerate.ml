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

let same_prefix e1 e2 =
  match e1, e2 with
  | Op1 (op1, e1'), Op1(op2, e2') -> op1 = op2
  | Op2 (op1, e1', _), Op2(op2, e2', _) -> op1 = op2 && e1' = e2'
  | _, _ -> false

let rec can_optimize_expr = function
  | Zero -> false
  | One -> false
  | Var id -> false
  | If0 (e1, e2, e3) ->
    can_optimize_expr e1 || can_optimize_expr e2 || can_optimize_expr e3 || e1 = Zero || e1 = One || e2 = e3 || same_prefix e2 e3
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

let rec enumerate_expr fold bound op1s op2s if0 n =
  if Hashtbl.mem memo (fold, bound, op1s, op2s, if0, n) then
    Hashtbl.find memo (fold, bound, op1s, op2s, if0, n)
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
                op <- List.enum op1s;
                e <- Set.enum (enumerate_expr fold bound op1s op2s if0 (n-1))
            ?];
            [? Set : Op2 (op, e1, e2) |
                op <- List.enum op2s;
                (x, y) <- split2 (n-1);
                e1 <- Set.enum (enumerate_expr fold bound op1s op2s if0 x);
                e2 <- Set.enum (enumerate_expr false bound op1s op2s if0 y)
            ?];
            [? Set : Op2 (op, e1, e2) |
                op <- List.enum op2s;
                (x, y) <- split2 (n-1);
                e1 <- Set.enum (enumerate_expr false bound op1s op2s if0 x);
                e2 <- Set.enum (enumerate_expr fold bound op1s op2s if0 y)
            ?];
            if if0 then
              [? Set : If0 (e1, e2, e3) |
                  (x, y, z) <- split3 (n-1);
                  e1 <- Set.enum (enumerate_expr fold bound op1s op2s if0 x);
                  e2 <- Set.enum (enumerate_expr false bound op1s op2s if0 y);
                  e3 <- Set.enum (enumerate_expr false bound op1s op2s if0 z)
              ?]
            else
              Set.empty;
            if if0 then
              [? Set : If0 (e1, e2, e3) |
                  (x, y, z) <- split3 (n-1);
                  e1 <- Set.enum (enumerate_expr false bound op1s op2s if0 x);
                  e2 <- Set.enum (enumerate_expr fold bound op1s op2s if0 y);
                  e3 <- Set.enum (enumerate_expr false bound op1s op2s if0 z)
              ?]
            else
              Set.empty;
            if if0 then
              [? Set : If0 (e1, e2, e3) |
                  (x, y, z) <- split3 (n-1);
                  e1 <- Set.enum (enumerate_expr false bound op1s op2s if0 x);
                  e2 <- Set.enum (enumerate_expr false bound op1s op2s if0 y);
                  e3 <- Set.enum (enumerate_expr fold bound op1s op2s if0 z)
              ?]
            else
              Set.empty;
            [? Set : Fold (e1, e2, e3) |
               (x, y, z) <- split3 (n-2);
               e1 <- Set.enum (enumerate_expr false false op1s op2s if0 x);
               e2 <- Set.enum (enumerate_expr false false op1s op2s if0 y);
               e3 <- Set.enum (enumerate_expr false true op1s op2s if0 z)
            ?]
          ]
        else
          Set.unions [
            [? Set : Op1 (op, e) |
                op <- List.enum op1s;
                e <- Set.enum (enumerate_expr fold bound op1s op2s if0 (n-1))
            ?];
            [? Set : Op2 (op, e1, e2) |
                op <- List.enum op2s;
                (x, y) <- split2 (n-1);
                e1 <- Set.enum (enumerate_expr fold bound op1s op2s if0 x);
                e2 <- Set.enum (enumerate_expr fold bound op1s op2s if0 y)
            ?];
            if if0 then
              [? Set : If0 (e1, e2, e3) |
                  (x, y, z) <- split3 (n-1);
                  e1 <- Set.enum (enumerate_expr fold bound op1s op2s if0 x);
                  e2 <- Set.enum (enumerate_expr fold bound op1s op2s if0 y);
                  e3 <- Set.enum (enumerate_expr fold bound op1s op2s if0 z)
              ?]
            else
              Set.empty
          ]
    end
    in
    Hashtbl.add memo (fold, bound, op1s, op2s, if0, n) ans; ans

let enumerate_tfold_program n op1s op2s fold if0 =
  [? Set : Lambda (Fold (e1, e2, e3)) |
     (x, y, z) <- split3 (n-2);
     e1 <- Set.enum (enumerate_expr false false op1s op2s if0 x);
     e2 <- Set.enum (enumerate_expr false false op1s op2s if0 y);
     e3 <- Set.enum (enumerate_expr false true op1s op2s if0 z)
  ?]

let enumerate_program n op1s op2s fold if0 =
  Set.map (fun e -> Lambda e) (enumerate_expr fold false op1s op2s if0 (n-1))

let rec f i candidates =
  prerr_endline ("enumerate.ml: size of candidates = " ^ (string_of_int (Set.cardinal candidates)));
  if Set.cardinal candidates < 100 || true then
    match guess (program_to_string (Set.choose candidates)) with
    | Win -> ()
    | Mismatch (input, expected, _) ->
      f (i+1) (Set.filter (fun p -> eval_program input p = expected) candidates)
  else
    let evalQ = List.init 256 (fun _ -> of_int (Random.int 0xfffffff)) in
    let evalA = eval evalQ in
    f (i+1) (List.fold_left (fun set (q, a) -> Set.filter (fun p -> eval_program q p = a) set) candidates (List.combine evalQ evalA))

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

let rec string_to_type = function
  | hd :: tl ->
    let op1s, op2s = string_to_type tl in
    if List.mem hd ["not"; "shl1"; "shr1"; "shr4"; "shr16"] then (string_to_op1 hd) :: op1s, op2s else
      if List.mem hd ["and"; "or"; "xor"; "plus"] then op1s, (string_to_op2 hd) :: op2s else op1s, op2s
  | [] -> [], []

let () =
  let Problem (n, ops) = get_problem () in
  let enumerate = if List.mem "tfold" ops then enumerate_tfold_program else enumerate_program in
  let op1s, op2s = string_to_type ops in
  let fold = List.mem "fold" ops or List.mem "tfold" ops in
  let if0 = List.mem "if0" ops in
  let candidates = [? Set : p | i <- (2--n); p <- Set.enum (enumerate i op1s op2s fold if0) ?]
  in
  if Array.mem "-v" Sys.argv then
    Set.iter (print_endline % program_to_string) candidates;
  prerr_endline (string_of_int (Set.cardinal candidates));
  f 0 candidates

(*
let () =
  print_endline (to_string_hex (eval_expr (of_int 0x1122334455667788) zero zero (Fold ((Var "x"), Zero, (Op2 (Or, (Var "y"), (Var "z")))))));
  flush stdout;
  Scanf.scanf "%d" solve*)
