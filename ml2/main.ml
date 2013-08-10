open Batteries
open MyStd
open Protocol
open Uint64

open Syntax

exception Todo

(*
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

let rec gen fold bound op1s op2s if0 n depth =
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
                e <- Set.enum (gen fold bound op1s op2s if0 (n-1))
            ?];
            [? Set : Op2 (op, e1, e2) |
                op <- List.enum op2s;
                (x, y) <- split2 (n-1);
                e1 <- Set.enum (gen fold bound op1s op2s if0 x);
                e2 <- Set.enum (gen false bound op1s op2s if0 y)
            ?];
            [? Set : Op2 (op, e1, e2) |
                op <- List.enum op2s;
                (x, y) <- split2 (n-1);
                e1 <- Set.enum (gen false bound op1s op2s if0 x);
                e2 <- Set.enum (gen fold bound op1s op2s if0 y)
            ?];
            if if0 then
              [? Set : If0 (e1, e2, e3) |
                  (x, y, z) <- split3 (n-1);
                  e1 <- Set.enum (gen fold bound op1s op2s if0 x);
                  e2 <- Set.enum (gen false bound op1s op2s if0 y);
                  e3 <- Set.enum (gen false bound op1s op2s if0 z)
              ?]
            else
              Set.empty;
            if if0 then
              [? Set : If0 (e1, e2, e3) |
                  (x, y, z) <- split3 (n-1);
                  e1 <- Set.enum (gen false bound op1s op2s if0 x);
                  e2 <- Set.enum (gen fold bound op1s op2s if0 y);
                  e3 <- Set.enum (gen false bound op1s op2s if0 z)
              ?]
            else
              Set.empty;
            if if0 then
              [? Set : If0 (e1, e2, e3) |
                  (x, y, z) <- split3 (n-1);
                  e1 <- Set.enum (gen false bound op1s op2s if0 x);
                  e2 <- Set.enum (gen false bound op1s op2s if0 y);
                  e3 <- Set.enum (gen fold bound op1s op2s if0 z)
              ?]
            else
              Set.empty;
            [? Set : Fold (e1, e2, e3) |
               (x, y, z) <- split3 (n-2);
               e1 <- Set.enum (gen false false op1s op2s if0 x);
               e2 <- Set.enum (gen false false op1s op2s if0 y);
               e3 <- Set.enum (gen false true op1s op2s if0 z)
            ?]
          ]
        else
          Set.unions [
            [? Set : Op1 (op, e) |
                op <- List.enum op1s;
                e <- Set.enum (gen fold bound op1s op2s if0 (n-1))
            ?];
            [? Set : Op2 (op, e1, e2) |
                op <- List.enum op2s;
                (x, y) <- split2 (n-1);
                e1 <- Set.enum (gen fold bound op1s op2s if0 x);
                e2 <- Set.enum (gen fold bound op1s op2s if0 y)
            ?];
            if if0 then
              [? Set : If0 (e1, e2, e3) |
                  (x, y, z) <- split3 (n-1);
                  e1 <- Set.enum (gen fold bound op1s op2s if0 x);
                  e2 <- Set.enum (gen fold bound op1s op2s if0 y);
                  e3 <- Set.enum (gen fold bound op1s op2s if0 z)
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
     e1 <- Set.enum (gen false false op1s op2s if0 x);
     e2 <- Set.enum (gen false false op1s op2s if0 y);
     e3 <- Set.enum (gen false true op1s op2s if0 z)
  ?]

*)

type c = uint64 * uint64
let c_dummy = (zero, zero)
let ones = lognot zero

let uint_to_c x =
  (lognot x, x)

let shift_left_with_one n m =
  logor (shift_left n m) (sub (shift_left one m) one)

let rec fold_or' init vec n =
  if n = 0 then
    init
  else
    fold_or' (logor (logand vec (of_int 0xff)) init) (shift_right vec 8) (n-1)

let fold_or vec =
  fold_or' zero vec 8

let rec eval (i0, i1) x y z expr =
  let (r0, r1) = match expr with
    | Zero -> (ones, zero)
    | One -> (zero, ones)
    | Var "x" -> x
    | Var "y" -> y
    | Var "z" -> z
    | Var _ -> (zero, zero)
    | Op1 (Not, e) ->
      let (o0, o1) = eval (i1, i0) x y z e in
      (o1, o0)
    | Op1 (Shl1, e) ->
      let shift n = 
        logor (shift_right n 1) (shift_left one 63) in
      let (o0, o1) = eval (shift i0, shift i1) x y z e in
      (shift_left o0 1, shift_left o1 1)
    | Op1 (Shr1, e) ->
      let (o0, o1) = eval (shift_left_with_one i0 1, shift_left_with_one i1 1) x y z e in
      (shift_right o0 1, shift_right o1 1)
    | Op1 (Shr4, e) ->
      let (o0, o1) = eval (shift_left_with_one i0 4, shift_left_with_one i1 4) x y z e in
      (shift_right o0 4, shift_right o1 4)
    | Op1 (Shr16, e) ->
      let (o0, o1) = eval (shift_left_with_one i0 16, shift_left_with_one i1 16) x y z e in
      (shift_right o0 16, shift_right o1 16)
    | Op2 (And, e1, e2) ->
      let (o0, o1) = eval (i0, ones) x y z e1 in
      let (p0, p1) = eval (i0, ones) x y z e2 in
      (logor o0 p0, logand o1 p1)
    | Op2 (Or, e1, e2) ->
      let (o0, o1) = eval (ones, i1) x y z e1 in
      let (p0, p1) = eval (ones, i1) x y z e2 in
      (logand o0 p0, logor o1 p1)
    | Op2 (Xor, e1, e2) ->
      let (o0, o1) = eval (ones, ones) x y z e1 in
      let (p0, p1) = eval (logor (logand i0 o0) (logand i1 o1),
                           logor (logand i0 o1) (logand i1 o0)) x y z e2 in
      (logor (logand o0 p0) (logand o1 p1), logor (logand o0 p1) (logand o1 p0))
    | Op2 (Plus, _, _) ->
      (ones, ones)
    | If0 (e1, e2, e3) ->
      let (o0, o1) = eval (ones, ones) x y z e1 in
      if o1 = zero then
        eval (i0, i1) x y z e2
      else if o0 <> ones then
        eval (i0, i1) x y z e3
      else
        let (p0, p1) = eval (i0, i1) x y z e2 in
        let (q0, q1) = eval (i0, i1) x y z e3 in
        (logor p0 q0, logor q0 q1)
    | Fold (e1, e2, e3) ->
      (*
      let (o0, o1) = eval (ones, ones) x y z e1 in
      let (p0, p1) = eval (ones, ones) x y z e2 in
      *)
      eval (ones, ones) x (fold_or i0, fold_or i1) (ones, ones) e3
    | Tree _ ->
      (ones, ones)
  in
  (logand i0 r0, logand i0 r1)

let rec gen w (size, fold, bound) =
  raise Todo

let rec expand = function
  | Zero -> Set.singleton Zero
  | One  -> Set.singleton One
  | Var id -> Set.singleton (Var id)
  | Op1 (op, e) -> 
    let es = expand e in
    Set.map (fun e' -> Op1 (op, e')) es
  | Op2 (op, e1, e2) ->
    let e1s = expand e1 in
    if Set.cardinal e1s <> 1 then
      Set.map (fun e1' -> Op2 (op, e1', e2)) e1s
    else
      let e2s = expand e2 in
      Set.map (fun e2' -> Op2 (op, Set.choose e1s, e2')) e2s
  | If0 (e1, e2, e3) ->
    let e1s = expand e1 in
    if Set.cardinal e1s <> 1 then
      Set.map (fun e1' -> If0 (e1', e2, e3)) e1s
    else 
      let e2s = expand e2 in
      if Set.cardinal e2s <> 1 then
        Set.map (fun e2' -> If0 (Set.choose e1s, e2', e3)) e2s
      else
        let e3s = expand e3 in
        Set.map (fun e3' -> If0 (Set.choose e1s, Set.choose e2s, e3')) e3s
  | Fold (e1, e2, e3) ->
    let e1s = expand e1 in
    if Set.cardinal e1s <> 1 then
      Set.map (fun e1' -> Fold (e1', e2, e3)) e1s
    else 
      let e2s = expand e2 in
      if Set.cardinal e2s <> 1 then
        Set.map (fun e2' -> Fold (Set.choose e1s, e2', e3)) e2s
      else
        let e3s = expand e3 in
        Set.map (fun e3' -> Fold (Set.choose e1s, Set.choose e2s, e3')) e3s
  | Tree chara ->
    gen 2 chara

let rec expand_n n expr =
  if n = 0 then 
    (Set.singleton expr : Syntax.expr Set.t)
  else
    [? Set : expr' | expr'' <- Set.enum (expand_n (n-1) expr); expr' <- Set.enum (expand expr'') ?] 

let eval c x y z = function
  | Zero -> raise Todo 
  | _ -> raise Todo

let can input expected expr =
  let (o0, o1) = 
    eval (uint_to_c expected) (uint_to_c input) c_dummy c_dummy expr 
  in
  lognot (logor o0 o1) = zero

let rec main op1s op2s if0 fold candidates qas =
  prerr_endline ("enumerate.ml: size of candidates = " ^ (string_of_int (Set.cardinal candidates)));

  let candidates = 
    [? Set : expr' | expr <- Set.enum candidates; expr' <- Set.enum (expand_n 3 expr) ?] 
  in
  let cs = Set.filter complete candidates in  
  if not (Set.is_empty cs) then
    let e, next_candidates = Set.pop cs
    in
    match guess (program_to_string (Lambda e)) with
    | Win -> ()
    | Unknown -> main op1s op2s if0 fold next_candidates qas
    | Mismatch (input, expected, _) ->
      main op1s op2s if0 fold
        (Set.filter (can input expected) candidates)
        (Set.add (input, expected) qas)
  else
    let q = Uint64.of_int64 (Random.int64 Int64.max_int) in
    let [a] = query [q] in
    main op1s op2s if0 fold
      (Set.filter (can q a) candidates)
      (Set.add (q, a) qas)

let rec string_to_type = function
  | hd :: tl ->
    let op1s, op2s = string_to_type tl in
    if List.mem hd ["not"; "shl1"; "shr1"; "shr4"; "shr16"] then (string_to_op1 hd) :: op1s, op2s else
      if List.mem hd ["and"; "or"; "xor"; "plus"] then op1s, (string_to_op2 hd) :: op2s else op1s, op2s
  | [] -> [], []

let () =
  let Problem (n, ops) = get_problem () in
  let op1s, op2s = string_to_type ops in
  let fold = List.mem "fold" ops or List.mem "tfold" ops in
  let if0 = List.mem "if0" ops in
  main op1s op2s fold if0
    [? Set : expr | size <- (1--(n-1)); expr <- Set.enum (expand_n 5 (Tree (size, fold, false))) ?] Set.empty

