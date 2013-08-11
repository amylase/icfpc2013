open Batteries

let first  f (x, y) = (f x,   y)
let second f (x, y) = (  x, f y)

let fst3 (x, _, _) = x
let snd3 (_, y, _) = y
let thd3 (_, _, z) = z

let first3  f (x, y, z) = (f x,   y,   z)
let second3 f (x, y, z) = (  x, f y,   z)
let third3  f (x, y, z) = (  x,   y, f z)

let ( ~~ ) x = flip x

let rec repeat n f x =
  if n = 0 then x
  else f (repeat (n-1) f x)

let rec power = function
  | [] -> [[]]
  | x :: xs ->
      let yss = power xs in
      yss @ List.map (fun ys -> x :: ys) yss

(*$T power
  power [] = [[]]
  power [1;2] = [[]; [2]; [1]; [1;2]]
*)

let rec n_cartesian_product = function
  | [] -> [[]]
  | h :: t ->
      let rest = n_cartesian_product t in
      List.concat (List.map (fun i -> List.map (fun r -> i :: r) rest) h)

(*$T n_cartesian_product as ncp
  ncp []               = [[]]
  ncp [[]]             = []
  ncp [[1]; [2]; [3]]  = [[1;2;3]]
  ncp [[1;2;3]]        = [[1]; [2]; [3]]
  ncp [[1;2;3]; []]    = []
  ncp [[1;2;3]; [4;5]] = [[1;4]; [1;5]; [2;4]; [2;5]; [3;4]; [3;5]]
*)

let concat_map f lst =
  List.concat (List.map f lst)

let invalid_argf fmt =
  Printf.ksprintf invalid_arg fmt

module Set = struct
  include Set

  let map f set =
    of_enum (Enum.map f (enum set))

  let map_to f set =
    Map.of_enum (Enum.map (fun k -> (k, f k)) (Set.enum set))

  let flatten sets =
    Set.fold Set.union sets Set.empty

  let unions lst =
    List.fold_left Set.union Set.empty lst

end

module Map = struct
  include Map

  let findDefault key def mp =
    try find key mp with Not_found -> def
end

module List = struct
  include List

  let zip_with_index lst =
    let rec f i = function
      | [] -> []
      | x :: xs -> (i, x) :: f (i+1) xs
    in
    f 0 lst

end
