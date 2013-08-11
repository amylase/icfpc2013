open Batteries
open MyStd
open Protocol
open Uint64

open Syntax

exception Todo

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
    can_optimize_expr e1 || can_optimize_expr e2 || can_optimize_expr e3 || e1 = Zero || e1 = One || same_prefix e2 e3
  | Fold (e1, e2, e3) ->
    can_optimize_expr e1 || can_optimize_expr e2 || can_optimize_expr e3
  | Op1 (op, e) ->
    can_optimize_expr e ||
      begin match op, e with
      | Not, Op1 (Not, _) -> true
      | _ -> false
      end
  | Op2 (op, e1, e2) ->
    can_optimize_expr e1 || can_optimize_expr e2 ||
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
  | Tree _ -> false

type c = uint64 * uint64
let c_dummy = (zero, zero)
let ones = lognot zero

let uint_to_c x =
  (lognot x, x)

let shift_left_with_one n m =
  logor (shift_left n m) (sub (shift_left one m) one)

let shift_right_with_one n m = 
  logor (shift_right n m) (shift_left (sub (shift_left one m) one) (64 - m))

(*
let cplus' n (n0, n1) (m0, m1) c0 c1 =
  let z = (sub (of_int 3) (add (add (of_int c0) (logand one n0)) (logand one m0))) in
  let o = (add (add (of_int c1) (logand one n1)) (logand one m1)) in
  match (z, o) with
    | 0 0 -> ()
  
  

let cplus (n0, n1) (m0, m1) =
  cplus' 64 (n0, n1) (m0, m1) 1 0
*)

let rec fold_or' init vec n =
  if n = 0 then
    init
  else
    fold_or' (logor (logand vec (of_int 0xff)) init) (shift_right vec 8) (n-1)

let fold_or vec =
  fold_or' zero vec 8

let rec uint_fold' f init vec n =
  if n = 0 then
    init
  else
    uint_fold' f (f (logand vec (of_int 0xff)) init) (shift_right vec 8) (n-1)

let uint_fold f init vec =
  uint_fold' f init vec 8

let plus_eval (o0, o1) (p0, p1) (r0, r1) =
  let max = add (add o1 p1) r1 in
  let isone (b0, b1) = logand b1 (lognot b0) in
  let min = add (add (isone (o0, o1)) (isone (p0, p1))) (isone (r0, r1)) in
  let two = of_int 2 in
  ((if min <= one then one else zero), (if max >= two then one else zero)),
  ((if max != min or rem min two = zero then one else zero), (if max != min or rem min two = one then one else zero))

let rec uint_fold_eval' i o p r n =
  if n = 0 then
    i
  else
    let pos = 64 - n in
    let i0, i1 = i in
    let get_bit (b0, b1) = logand one (shift_right b0 pos), logand one (shift_right b1 pos) in
    let nr, (ni0, ni1) = plus_eval (get_bit o) (get_bit p) r in
    let ni = logor i0 (shift_left ni0 pos), logor i1 (shift_left ni1 pos) in
    uint_fold_eval' ni o p nr (n-1)

let uint_fold_eval o p =
  uint_fold_eval' (zero, zero) o p (zero, zero) 64

let rec eval (i0, i1) x y z expr =
  let (r0, r1) = match expr with
    | Zero -> (ones, zero)
    | One -> (lognot one, one)
    | Var "x" -> x
    | Var "y" -> y
    | Var "z" -> z
    | Var _ -> (zero, zero)
    | Op1 (Not, e) ->
      let (o0, o1) = eval (i1, i0) x y z e in
      (o1, o0)
    | Op1 (Shl1, e) ->
      let (o0, o1) = eval (shift_right_with_one i0 1, shift_right_with_one i1 1) x y z e in
      (shift_left_with_one o0 1, shift_left o1 1)
    | Op1 (Shr1, e) ->
      let (o0, o1) = eval (shift_left_with_one i0 1, shift_left_with_one i1 1) x y z e in
      (shift_right_with_one o0 1, shift_right o1 1)
    | Op1 (Shr4, e) ->
      let (o0, o1) = eval (shift_left_with_one i0 4, shift_left_with_one i1 4) x y z e in
      (shift_right_with_one o0 4, shift_right o1 4)
    | Op1 (Shr16, e) ->
      let (o0, o1) = eval (shift_left_with_one i0 16, shift_left_with_one i1 16) x y z e in
      (shift_right_with_one o0 16, shift_right o1 16)
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
    | Op2 (Plus, e1, e2) ->
      let (o0, o1) = eval (ones, ones) x y z e1 in
      let (p0, p1) = eval (ones, ones) x y z e2 in
      if (logxor o0 o1) = ones && (logxor p0 p1) = ones then
        (lognot (add o1 p1), add o1 p1)
      else
        (* (ones, ones) *) uint_fold_eval (o0, o1) (p0, p1)
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
      let (o0, o1) = eval (ones, ones) x y z e1 in
      let (p0, p1) = eval (ones, ones) x y z e2 in
      if (logxor o0 o1) = ones && (logxor p0 p1) = ones then
        let ans = uint_fold (fun y z -> (snd (eval (ones, ones) x (lognot y, y) (lognot z, z) e3))) p1 o1 in
        (lognot ans, ans)
      else
        eval (ones, ones) x (fold_or i0, fold_or i1) (ones, ones) e3
    | Tree _ ->
      (ones, ones)
  in
  (logand i0 r0, logand i1 r1)

(*$T eval
  eval (ones, ones) (uint_to_c Uint64.one) c_dummy c_dummy Syntax.Zero = (uint_to_c Uint64.zero)
  eval (ones, ones) (uint_to_c Uint64.one) c_dummy c_dummy Syntax.One = (uint_to_c Uint64.one)
  eval (ones, ones) (uint_to_c (Uint64.of_int 12)) c_dummy c_dummy (Syntax.Var "x") = (uint_to_c (Uint64.of_int 12))
  eval (ones, ones) c_dummy c_dummy (uint_to_c (Uint64.of_int 999)) (Syntax.Var "z") = (uint_to_c (Uint64.of_int 999))
  eval (uint_to_c (Uint64.of_int 7)) (uint_to_c (Uint64.of_int 15)) c_dummy c_dummy (Syntax.Op2 (Syntax.And, Syntax.Var "x", Syntax.Tree (5, true, true))) = c_dummy
*)

let memo = Hashtbl.create 0

let rec gen w (size, fold, bound) (op1s : Syntax.op1 list) op2s if0 =
  if Hashtbl.mem memo (w, size, fold, bound) then
    Hashtbl.find memo (w, size, fold, bound)
  else 
    if w = 0 then
      Set.singleton (Tree (size, fold, bound))
    else
      let ans =
        match size with
          | 0 -> invalid_arg "zero is not allowed."
          | 1 -> 
            Set.of_list
            (if bound then [Zero; One; Var "x"; Var "y"; Var "z"] else [Zero; One; Var "x"])
          | n ->
            Set.filter (not % can_optimize_expr) begin
              if fold then
                Set.unions [
                  [? Set : Op1 (op, e) |
                      op <- List.enum op1s;
                   e <- Set.enum (gen (w-1) (size-1, fold, bound) op1s op2s if0)
                  ?];
                  [? Set : Op2 (op, e1, Tree (y, false, bound)) |
                      op <- List.enum op2s;
                   (x, y) <- split2 (n-1);
                   x <= y;
                   e1 <- Set.enum (gen (w-1) (x, fold, bound) op1s op2s if0)
                  ?];
                  [? Set : Op2 (op, e1, Tree (y, fold, bound)) |
                      op <- List.enum op2s;
                   (x, y) <- split2 (n-1);
                   x <= y;
                   e1 <- Set.enum (gen (w-1) (x, false, bound) op1s op2s if0)
                  ?];
                  if if0 then
                    [? Set : If0 (e1, Tree (y, b2, bound), Tree (z, b3, bound)) |
                        (x, y, z) <- split3 (n-1);
                     (b1, b2, b3) <- List.enum [(fold, false, false); (false, fold, false); (false, false, fold) ];
                     e1 <- Set.enum (gen (w-1) (x, b1, bound) op1s op2s if0)
                    ?]
                  else
                    Set.empty;
                  [? Set : Fold (e1, Tree (y, false, false), Tree (z, false, true)) |
                      (x, y, z) <- split3 (n-2);
                   e1 <- Set.enum (gen (w-1) (x, false, false) op1s op2s if0)
                  ?]
                ]
              else
                Set.unions [
                  [? Set : Op1 (op, e) |
                      op <- List.enum op1s;
                   e <- Set.enum (gen (w-1) (n-1, fold, bound) op1s op2s if0)
                     ?];
                  [? Set : Op2 (op, e1, Tree (y, fold, bound)) |
                      op <- List.enum op2s;
                   (x, y) <- split2 (n-1);
                   x <= y;
                   e1 <- Set.enum (gen (w-1) (x, fold, bound) op1s op2s if0)
                     ?];
                  if if0 then
                    [? Set : If0 (e1, Tree (y, fold, bound), Tree (z, fold, bound)) |
                        (x, y, z) <- split3 (n-1);
                     e1 <- Set.enum (gen (w-1) (x, fold, bound) op1s op2s if0)
                       ?]
                  else
                    Set.empty
                ]
            end
      in
      Hashtbl.add memo (w, size, fold, bound) ans; ans
      
let rec expand op1s op2s if0 = function
  | Zero -> Set.singleton Zero
  | One  -> Set.singleton One
  | Var id -> Set.singleton (Var id)
  | Op1 (op, e) -> 
    let es = expand op1s op2s if0 e in
    Set.map (fun e' -> Op1 (op, e')) es
  | Op2 (op, e1, e2) ->
    let e1s = expand op1s op2s if0 e1 in
    if Set.cardinal e1s <> 1 then
      Set.map (fun e1' -> Op2 (op, e1', e2)) e1s
    else
      let e2s = expand op1s op2s if0 e2 in
      Set.map (fun e2' -> Op2 (op, Set.choose e1s, e2')) e2s
  | If0 (e1, e2, e3) ->
    let e1s = expand op1s op2s if0 e1 in
    if Set.cardinal e1s <> 1 then
      Set.map (fun e1' -> If0 (e1', e2, e3)) e1s
    else 
      let e2s = expand op1s op2s if0 e2 in
      if Set.cardinal e2s <> 1 then
        Set.map (fun e2' -> If0 (Set.choose e1s, e2', e3)) e2s
      else
        let e3s = expand op1s op2s if0 e3 in
        Set.map (fun e3' -> If0 (Set.choose e1s, Set.choose e2s, e3')) e3s
  | Fold (e1, e2, e3) ->
    let e1s = expand op1s op2s if0 e1 in
    if Set.cardinal e1s <> 1 then
      Set.map (fun e1' -> Fold (e1', e2, e3)) e1s
    else 
      let e2s = expand op1s op2s if0 e2 in
      if Set.cardinal e2s <> 1 then
        Set.map (fun e2' -> Fold (Set.choose e1s, e2', e3)) e2s
      else
        let e3s = expand op1s op2s if0 e3 in
        Set.map (fun e3' -> Fold (Set.choose e1s, Set.choose e2s, e3')) e3s
  | Tree chara ->
    gen 1 chara op1s op2s if0

let rec expand_n n expr op1s op2s if0 =
  if n = 0 then 
    (Set.singleton expr : Syntax.expr Set.t)
  else
    [? Set : expr' | expr'' <- Set.enum (expand_n (n-1) expr op1s op2s if0); expr' <- Set.enum (expand op1s op2s if0 expr'') ?] 

let printc (i0, i1) =
  prerr_endline ("(" ^ (to_string_bin i0) ^ ", " ^ to_string_bin i1 ^ ")")

let can input expected expr =
  let (o0, o1) = 
    eval (uint_to_c expected) (uint_to_c input) c_dummy c_dummy expr 
  in
  lognot (logor o0 o1) = zero

let rec main op1s op2s if0 candidates qas =
  prerr_endline ("enumerate.ml: size of candidates = " ^ (string_of_int (Set.cardinal candidates)));
  (*Set.iter (prerr_endline % expr_to_string) candidates;*)
  let will_expand = BatEnum.take 50000 (Set.enum candidates) in
  let candidates = Set.of_enum (BatEnum.skip 50000 (Set.enum candidates)) in
  let candidates =
    Set.filter (fun candidate -> Set.for_all (fun (q, a) -> can q a candidate) qas)
    (Set.union
    [? Set : expr' | expr <- will_expand; expr' <- Set.enum (expand_n 2 expr op1s op2s if0) ?]
      candidates
    )
     in
  prerr_endline ("candidates after expansion = " ^ (string_of_int (Set.cardinal candidates)));
  let cs = Set.filter complete candidates in  
  prerr_endline ("complete candidates = " ^ (string_of_int (Set.cardinal cs)));
  if ((Set.cardinal candidates) >= 10000 || (Set.cardinal cs) = (Set.cardinal candidates)) && (Set.cardinal cs) <> 0 then
    let e, next_candidates = Set.pop cs
    in
    match guess (program_to_string (Lambda e)) with
    | Win -> ()
    | Unknown -> main op1s op2s if0 (Set.remove e candidates) qas
    | Mismatch (input, expected, _) ->
      main op1s op2s if0
        (Set.filter (can input expected) candidates)
        (Set.add (input, expected) qas)
  else
    let q = Uint64.of_int64 (Random.int64 Int64.max_int) in
    let [a] = query [q] in
    let newqas = (Set.add (q, a) qas) in
    main op1s op2s if0
      (Set.filter (fun candidate -> Set.for_all (fun (q, a) -> can q a candidate) newqas) candidates) newqas

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
  main op1s op2s if0
    [? Set : expr | size <- (1--(n-1)); expr <- Set.enum (expand_n 5 (Tree (size, fold, false)) op1s op2s if0) ?] Set.empty


(*
let () = 
  let printc (i0, i1) =
    print_endline ("(" ^ (to_string_bin i0) ^ ", " ^ to_string_bin i1 ^ ")") in
  printc (eval (ones, ones) (uint_to_c Uint64.one) c_dummy c_dummy Syntax.Zero);
  printc (eval (ones, ones) (uint_to_c Uint64.one) c_dummy c_dummy Syntax.One);
  printc (eval (ones, ones) (uint_to_c (Uint64.of_int 12)) c_dummy c_dummy (Syntax.Var "x"));
  printc (eval (ones, ones) c_dummy c_dummy (uint_to_c (Uint64.of_int 999)) (Syntax.Var "z"));
  printc (eval (uint_to_c (Uint64.of_int 7)) (uint_to_c (Uint64.of_int 3)) c_dummy c_dummy (Syntax.Op2 (Syntax.And, Syntax.Var "x", Syntax.Tree (5, true, true))))
*)
