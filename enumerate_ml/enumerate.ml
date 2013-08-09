open Batteries_uni
open MyStd

type id = string
type op1 = Not | Shl1 | Shr1 | Shr4 | Shr16
type op2 = And | Or | Xor | Plus
type expr =
| Zero
| One
| Var of id
| If0 of expr * expr * expr
| Fold of expr * expr * id * id * expr
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
  | Fold (e1, e2, id1, id2, e3) -> "fold " ^ expr_to_string e1 ^ " " ^ expr_to_string e2 ^ " (lambda (" ^ id1 ^ ", " ^ id2 ^ ") " ^ expr_to_string e3 ^ ")"
  | Op1 (op, e) -> "(" ^ op1_to_string op ^ " " ^ expr_to_string e ^ ")"
  | Op2 (op, e1, e2) -> "(" ^ op2_to_string op ^ " " ^ expr_to_string e1 ^ " " ^ expr_to_string e2 ^ ")"

let program_to_string (Lambda e) =
  "(lambda (x) " ^ expr_to_string e ^ ")"

let _cnt = ref 0

let new_id () =
  incr _cnt;
  "x_" ^ (string_of_int (!_cnt))

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

let rec optimize_expr = function
  | Zero -> Zero
  | One -> One
  | Var id -> Var id
  | If0 (e1, e2, e3) ->
    let e1' = optimize_expr e1 in
    let e2' = optimize_expr e2 in
    let e3' = optimize_expr e3 in
    begin match optimize_expr e1' with
    | Zero -> e2
    | One  -> e3
    | _ -> if e2' = e3' then e2' else If0 (e1', e2', e3')
    end
  | Fold (e1, e2, id1, id2, e3) ->
    let e1' = optimize_expr e1 in
    let e2' = optimize_expr e2 in
    let e3' = optimize_expr e3 in
    Fold (e1', e2', id1, id2, e3')
  | Op1 (op, e) ->
    let e' = optimize_expr e in
    begin match op, e' with
    | Not, Op1 (Not, e'') -> e''
    | _ -> e'
    end
  | Op2 (op, e1, e2) ->
    let e1' = optimize_expr e1 in
    let e2' = optimize_expr e2 in
    let e1'', e2'' =
      if e1' < e2' then
        e1', e2'
      else
        e2', e1'
    in
    Op2 (op, e1'', e2'')

let optimize_program (Lambda e) = Lambda (optimize_expr e)

let memo = Hashtbl.create 0

let rec enumerate_expr ids n =
  if Hashtbl.mem memo (ids, n) then
    Hashtbl.find memo (ids, n)
  else
    let ans = Set.map optimize_expr begin match n with
      | 0 -> invalid_arg "zero is not allowed"
      | 1 -> Set.of_list (List.rev_append [Zero; One] (List.map (fun id -> Var id) ids))
      | n ->
        let id1 = new_id () in
        let id2 = new_id () in
        Set.unions [
          [? Set : Op1 (op, e) |
           op <- List.enum [Not; Shl1; Shr1; Shr4; Shr16];
           e <- Set.enum (enumerate_expr ids (n-1))
          ?];
          [? Set : Op2 (op, e1, e2) |
           op <- List.enum [And; Or; Xor; Plus];
           (x, y) <- split2 (n-1);
           e1 <- Set.enum (enumerate_expr ids x);
           e2 <- Set.enum (enumerate_expr ids y)
          ?];
          [? Set : If0 (e1, e2, e3) |
           (x, y, z) <- split3 (n-1);
           e1 <- Set.enum (enumerate_expr ids x);
           e2 <- Set.enum (enumerate_expr ids y);
           e3 <- Set.enum (enumerate_expr ids z)
          ?];
          [? Set : Fold (e1, e2, id1, id2, e3) |
           (x, y, z) <- split3 (n-2);
           e1 <- Set.enum (enumerate_expr ids x);
           e2 <- Set.enum (enumerate_expr ids y);
           e3 <- Set.enum (enumerate_expr (id1 :: id2 :: ids) z)
          ?];
        ]
    end
    in
    Hashtbl.add memo (ids, n) ans; ans

let enumerate_program n =
  Set.map (fun e -> Lambda e) (enumerate_expr ["x"] (n-1))

let solve n =
  let ans = enumerate_program n
  in
  (*Set.iter (print_endline % program_to_string) ans';*)
  print_endline (string_of_int (Set.cardinal ans))

let () =
  Scanf.scanf "%d" solve
