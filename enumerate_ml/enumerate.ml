open MyBatteries
open MyStd

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

let rec enumerate_expr fold n =
  if Hashtbl.mem memo (fold, n) then
    Hashtbl.find memo (fold, n)
  else
    let ans = Set.filter (not % can_optimize_expr) begin match n with
      | 0 -> invalid_arg "zero is not allowed"
      | 1 ->
        Set.of_list
          (if fold then [Zero; One; Var "x"] else [Zero; One; Var "x"; Var "y"; Var "z"])
      | n ->
        Set.unions [
          [? Set : Op1 (op, e) |
           op <- List.enum [Not; Shl1; Shr1; Shr4; Shr16];
           e <- Set.enum (enumerate_expr fold (n-1))
          ?];
          [? Set : Op2 (op, e1, e2) |
           op <- List.enum [And; Or; Xor; Plus];
           (x, y) <- split2 (n-1);
           e1 <- Set.enum (enumerate_expr fold x);
           e2 <- Set.enum (enumerate_expr fold y)
          ?];
          [? Set : If0 (e1, e2, e3) |
           (x, y, z) <- split3 (n-1);
           e1 <- Set.enum (enumerate_expr fold x);
           e2 <- Set.enum (enumerate_expr fold y);
           e3 <- Set.enum (enumerate_expr fold z)
          ?];
          if fold then
            [? Set : Fold (e1, e2, e3) |
               (x, y, z) <- split3 (n-2);
               e1 <- Set.enum (enumerate_expr fold x);
               e2 <- Set.enum (enumerate_expr fold y);
               e3 <- Set.enum (enumerate_expr false z)
            ?]
          else
            Set.empty
        ]
    end
    in
    Hashtbl.add memo (fold, n) ans; ans

let enumerate_program n =
  Set.map (fun e -> Lambda e) (enumerate_expr true (n-1))

let solve n =
  let ans = enumerate_program n
  in
  if Array.mem "-v" Sys.argv then
    Set.iter (print_endline % program_to_string) ans;
  print_endline (string_of_int (Set.cardinal ans))

let () =
  Scanf.scanf "%d" solve
